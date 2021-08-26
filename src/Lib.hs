{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Mueval.Interpreter
import Mueval.ArgsParse
import Language.Haskell.Interpreter
import Mueval.Context
import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser
    ( callbackQueryDataRead, command, parseUpdate )
import GHC.Conc ( killThread, threadDelay, forkIO )
import Control.Applicative ( Alternative((<|>)) )
import Control.Monad.IO.Class (MonadIO(liftIO))
import Mueval.Parallel (watchDog)
import qualified Data.Text as T
import Data.Text (Text)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, swapMVar, tryPutMVar)
import Control.Exception.Base (catch, SomeException (SomeException), AsyncException (ThreadKilled), throw)
import qualified Data.Bifunctor as BF
import Control.Exception (SomeException(SomeException))
import GHC.Exception (SomeException(SomeException))
import Control.Monad ( MonadPlus(mplus), when, mfilter )
import Data.List (stripPrefix)
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)

type Model = ()
data Action =
     NoOp 
     | Start 
     | Eval Text
     deriving (Show, Read)

swapThird :: (a, b, c) -> c -> (a, b, c)
swapThird (a, b, _) c = (a, b, c)

replace :: Eq b => b -> b -> [b] -> [b]
replace a b = map $ fromMaybe b . mfilter (/= a) . Just

defaultOptions :: Options
defaultOptions = Options { expression = ""
                           , modules = Just defaultModules
                           , timeLimit = 5
                           , user = ""
                           , loadFile = ""
                           , printType = False
                           , typeOnly = False
                           , extensions = False
                           , namedExtensions = []
                           , noImports = False
                           , rLimits = False
                           , packageTrust = False 
                           , trustedPackages = defaultPackages
                           , help = False }

formatResult :: (String, String, String) -> Text
formatResult (expr, etype, result) = T.unlines [
        T.concat $ map T.pack 
            ["<b>Expr:</b> <code>", expr, "</code> of type <code>", etype, "</code>"],
        T.concat ["<b>Result:</b> <code>", T.pack result, "</code>"]
    ]

formatError :: Text -> Text
formatError a = T.concat ["<b>Error encountered: </b><code>", a, "</code>"]

showInterpreterError :: InterpreterError -> Text
showInterpreterError (WontCompile errs) =
        fromMaybe errorText $ T.stripPrefix "error: " errorText
    where
        errorText = T.replace "<" "{|" $ T.replace ">" "|}" 
                        $ T.pack $ concatMap (dropLinePosition . errMsg) errs
        dropLinePosition e
          | Just s <- parseErr e =  s
          | otherwise = e -- if the parse fails we fallback on printing the whole error
        parseErr e = do s <- stripPrefix "<interactive>:" e
                        skipSpaces =<< (skipNumber =<< skipNumber s)
        skip x (y:xs) | x == y = Just xs
                        | otherwise = Nothing
        skip _ _ = Nothing
        skipNumber = skip ':' . dropWhile isDigit
        skipSpaces xs = let xs' = dropWhile (==' ') xs
                        in skip '\n' xs' `mplus` return xs'

evalBot = let
    updateToAction :: Model -> Update -> Maybe Action
    updateToAction _ = parseUpdate $
            Start <$  command "start"
        <|> Eval  <$> command "heval"
        <|> callbackQueryDataRead 
    
    handleAction :: Action -> Model -> Eff Action Model
    handleAction action model = case action of 
        NoOp -> pure model
        Start -> model <# do
            replyText "TODO"
            return NoOp
        Eval input -> model <# do 
            text <- liftIO $ do
                putStrLn $ "Evaluating expression " ++ T.unpack input
                var <- newEmptyMVar
                id <- forkIO $
                    (BF.first showInterpreterError <$>
                        runInterpreter (interpreter defaultOptions { expression = T.unpack input }))
                            `catch` (\e -> throw (e :: AsyncException))
                            `catch` (\e -> return . Left . T.append "Exception: " . T.pack $ show (e :: SomeException))
                    >>= putMVar var . Left

                forkIO $ do
                    threadDelay 5000000 -- 5 seconds
                    kill <- tryPutMVar var $ Right ()
                    when kill $ do
                        putStrLn "Killing thread"
                        killThread id

                val <- takeMVar var
                putMVar var $ Right ()

                case val of
                    Left res -> case res of 
                        Right res -> do
                            (out, b) <- render 1024 $ (\(e, et, r) -> r) res
                            return $ if b then formatError "Exception encountered while parsing data" 
                                        else formatResult $ swapThird res $
                                            if length out >= 1024 then out ++ "{| Output too big |}" else out
                        Left err -> return $ formatError err
                    Right _ -> return $ formatError "Computation timed out"

            liftIO $ putStrLn $ "Sending " ++ T.unpack text

            reply $ (toReplyMessage text) { replyMessageParseMode = Just HTML  }
            return NoOp
    in BotApp { botInitialModel = ()
                , botAction = flip updateToAction
                , botHandler = handleAction
                , botJobs = []
                }


run :: Token -> IO ()
run token = do
    env <- defaultTelegramClientEnv token
    startBot_ (conversationBot updateChatId evalBot) env

botMain :: IO ()
botMain = do
    putStrLn "Starting the bot"
    token <- getEnvToken "TELEGRAM_BOT_TOKEN"
    run token
