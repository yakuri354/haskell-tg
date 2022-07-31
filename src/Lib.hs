{-# LANGUAGE OverloadedStrings, LambdaCase, TupleSections #-}

module Lib where

import Mueval.Interpreter
import Mueval.ArgsParse
import Language.Haskell.Interpreter
import Mueval.Context
import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser
    ( callbackQueryDataRead, command, parseUpdate, plainText, UpdateParser (UpdateParser) )
import GHC.Conc ( killThread, threadDelay, forkIO )
import Control.Applicative ( Alternative((<|>)) )
import Control.Monad.IO.Class (MonadIO(liftIO))
import Mueval.Parallel (watchDog)
import Data.Functor ( (<&>) )
import qualified Data.Text as T
import Data.Text (Text)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, swapMVar, tryPutMVar)
import Control.Exception.Base (catch, SomeException (SomeException), AsyncException (ThreadKilled), throw)
import qualified Data.Bifunctor as BF
import Control.Exception (SomeException(SomeException))
import GHC.Exception (SomeException(SomeException))
import Control.Monad ( MonadPlus(mplus), when, mfilter, unless )
import Data.List (stripPrefix)
import Data.Function ( (&) )
import Data.Char (isDigit, isPrint)
import Data.Maybe (fromMaybe, isJust, fromJust)
import System.Process (readProcess, readProcessWithExitCode)
import System.Exit (ExitCode(ExitSuccess))
import GHC.IO.Exception (ExitCode(ExitFailure))
import Telegram.Bot.Simple.Reply (ReplyMessage(replyMessageReplyToMessageId))
import Telegram.Bot.API.InlineMode.InlineQueryResult (InlineQueryResultType(InlineQueryResultArticle), InlineQueryResult (inlineQueryResultId, InlineQueryResult), InlineQueryResultId (InlineQueryResultId))
import Telegram.Bot.API.InlineMode.InputMessageContent (defaultInputTextMessageContent, InputMessageContent (InputTextMessageContent))
import Test.RandomStrings (randomASCII, randomString)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Servant.Client (ClientM)
import Crypto.Hash.Algorithms
import Crypto.Hash
import qualified Telegram.Bot.API as API
import Telegram.Bot.API.InlineMode
import Data.Memory.Encoding.Base32 (toBase32)
import Data.ByteArray.Encoding (convertToBase, Base (Base64, Base32))
import Telegram.Bot.API.Types

import Unicode

type Model = ()
data Action =
     NoOp 
     | Start
     | InlineQ InlineQueryId Text
     | InlineChosen ChosenInlineResult
     | Eval Text
     | Callback CallbackQuery
     deriving (Show)

muevalOpts = ["-t", "5", "-i"]
typeOnlyOpts = ["-T"]

dummyMarkup = SomeInlineKeyboardMarkup $
    InlineKeyboardMarkup { 
        inlineKeyboardMarkupInlineKeyboard = [
            [callbackButton "Evaluating..." "BUTTONCLICKED"]
        ] 
    }

hashContent :: Text -> Text
hashContent text = T.take 64 $ decodeUtf8 $ convertToBase Base32 (hash $ encodeUtf8 text :: Digest SHA384)

boldMD :: Text -> Text
boldMD text = T.concat ["*", T.replace "*" "\\*" text, "*"]

codeMD :: Text -> Text
codeMD text = T.concat ["`", T.replace "`" "\\`" (
        T.replace "\\" "\\\\" text
    ), "`"]

messageTextAndPrivate :: Message -> Maybe (Bool, Text)
messageTextAndPrivate m = messageText m <&>
    (case m & chatType . messageChat of
        ChatTypePrivate -> True
        _ -> False,)

parseOutput :: Text -> Text
parseOutput raw =
    let lines = T.lines raw in 
        if length lines /= 3 then raw  -- If there are not 3 lines, fall back to displaying everything
        else T.unlines [
            T.concat [boldMD "Expression: ", codeMD $ head lines],
            T.concat [boldMD "Type: ", codeMD $ lines !! 1],
            "", -- An extra newline
            T.concat [boldMD "Result: ", codeMD $ last lines]
        ]

evalPending :: Text -> Text
evalPending input =
    boldMD "Expression: " `T.append` codeMD input

answerInline :: Maybe Text -> Maybe Text -> Maybe SomeReplyMarkup -> InlineQueryId -> ClientM (Response Bool, Text)
answerInline title content markup query = 
    (, msgHash) 
        <$> answerInlineQuery
            (AnswerInlineQueryRequest query [
                InlineQueryResult
                    InlineQueryResultArticle 
                    (InlineQueryResultId msgHash)
                    title
                    ((\x -> InputTextMessageContent x (Just "MarkdownV2") (Just False))
                        <$> content)
                    markup
            ])
    where
        msgHash = hashContent $ fromMaybe "empty" content

invokeMueval :: Text -> [String] -> IO Text
invokeMueval input opts = do
            (code, out, err) <- readProcessWithExitCode "mueval" (opts ++ ["-e", T.unpack input]) ""
            return $ case code of 
                    ExitSuccess -> (parseOutput . T.pack . uniConv) out
                    ExitFailure _ -> T.unlines [
                            boldMD "Expression: " `T.append` codeMD (T.strip input),
                            "",
                            (codeMD . T.pack . uniConv) $ chooseOne out err "Internal error occured while evaluating"
                        ]
            where
                chooseOne a b c
                    | not $ null a = a
                    | not $ null b = b
                    | otherwise = c

evalBot :: BotApp () Action
evalBot = let
    updateToAction :: Model -> Update -> Maybe Action
    updateToAction model update
        | isJust $ updateInlineQuery update = do
            query <- updateInlineQuery update
            let queryId = inlineQueryId query
            let msg = inlineQueryQuery query
            Just $ InlineQ queryId msg
        | isJust $ updateChosenInlineResult update = 
            updateChosenInlineResult update >>= Just . InlineChosen 
        | isJust $ updateCallbackQuery update =
            updateCallbackQuery update >>= Just . Callback
        | otherwise = updateMessage update
                        >>= messageTextAndPrivate 
                        >>= parseRaw
            where parseRaw (private, txt) = 
                        Eval <$> T.stripPrefix "/heval " txt
                    <|> if txt == "/start" 
                        then Just Start 
                        else Nothing 
                    <|> if private && not (T.null txt)
                        then Just $ Eval txt 
                        else Nothing 
            
    handleAction :: Action -> Model -> Eff Action Model
    handleAction action model = case action of 
        NoOp -> pure model
        Start -> model <# do
            replyText "TODO" -- TODO
            return NoOp
        Eval input -> model <# do 
            chatID <- fromJust <$> currentChatId 
            msgID <- getEditMessageId <&> \x -> x <&> \case
                EditChatMessageId _ msg -> msg
                EditInlineMessageId _ -> undefined -- fixme
            liftIO $ putStrLn $ "Expr: " ++ T.unpack input ++ " from " ++ show chatID
            res <- liftIO $ invokeMueval input muevalOpts
            reply (toReplyMessage res) { 
                  replyMessageReplyToMessageId = msgID
                , replyMessageParseMode = Just MarkdownV2 
                }
            return NoOp
        InlineQ query input -> model <# do
            if T.null (T.strip input) then do
                liftIO $ putStrLn "Empty inline query received"
                (sent, _) <- liftClientM $ answerInline (Just "Type the expression") Nothing Nothing query
                unless (responseResult sent) $ liftIO $ putStrLn "WARNING: Inline query not sent"
            else do
                liftIO $ putStrLn $ "Inline query: " ++ T.unpack input
                (sent, msgId) <- liftClientM $ answerInline (Just "Evaluate") (Just $ evalPending input) (Just dummyMarkup) query
                unless (responseResult sent) $ liftIO $ putStrLn "WARNING: Inline query not sent"
            return NoOp
        InlineChosen ch -> model <# do
            let input = chosenInlineResultQuery ch
            liftIO $ putStrLn $ "Evaluating: " ++ T.unpack input
            res <- liftIO $ invokeMueval input muevalOpts
            liftIO $ putStrLn $ "Result: \n" ++ T.unpack res
            m <- liftClientM $
                API.editMessageText $ EditMessageTextRequest 
                    Nothing 
                    Nothing
                    (chosenInlineResultInlineMessageId ch) 
                    res 
                    (Just MarkdownV2)
                    Nothing
                    Nothing
            return NoOp
        Callback cb -> model <# do
            liftClientM $ answerCallbackQuery $ AnswerCallbackQueryRequest 
                (callbackQueryId cb) 
                (Just "Wait for the expression to evaluate")
                Nothing
                Nothing
                Nothing
            return NoOp
    in BotApp { botInitialModel = ()
                , botAction = flip updateToAction
                , botHandler = handleAction
                , botJobs = []
                }


run :: Token -> IO ()
run token = do
    env <- defaultTelegramClientEnv token
    res <- startBot evalBot env
    case res of
        Left err -> putStrLn "ERROR: \n" >> print err
        Right _ -> return ()

botMain :: IO ()
botMain = do
    putStrLn "Starting the bot"
    token <- getEnvToken "TELEGRAM_BOT_TOKEN"
    run token
