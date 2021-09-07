module Unicode (uniConv) where

import Text.ParserCombinators.ReadP
import Text.Read.Lex (lexChar)
import Data.Char (isPrint)
import Control.Applicative ( Alternative((<|>)) )

type Replacement = (String, String)

recoverChar :: (Char -> Bool) -> ReadP Replacement
recoverChar p = (represent <$> gather lexChar) <|> (("\\&","\&") <$ string "\\&")
  where
    represent :: (String, Char) -> Replacement
    represent (o,lc)
      | p lc      = (o, [lc])
      | otherwise = (o, o)

uniConv :: String -> String
uniConv = go ("", "") . readP_to_S (many $ recoverChar p)
  where
    p c = isPrint c && notElem c ['\\', '\'','\"']
    go :: Replacement -> [([Replacement], String)] -> String
    go _  []            = ""
    go _  (([],""):_)   = ""
    go _  ((rs,""):_)   = snd $ last rs
    go _  [(_,o)]    = o
    go pr (([],_):rest) = go pr rest
    go _  ((rs,_):rest) = let r = last rs in snd r ++ go r rest