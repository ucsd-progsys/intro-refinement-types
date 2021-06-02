{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-@ LIQUID "--no-termination" @-}

module Figures where

import Data.IORef
import Text.Pandoc.JSON
-- import Text.Pandoc
import Text.Pandoc.Walk -- (walkM)
-- import  Data.List (isSuffixOf, isPrefixOf)
-- import Debug.Trace
-- import Text.Printf (printf)

import System.FilePath (takeFileName, takeExtension)
-- import Data.Monoid (mempty)
-- import System.Environment (getEnv)

-- import System.Directory
-- import System.IO
-- import Control.Applicative ((<$>))
import Control.Monad (replicateM_)

-- import qualified Data.ByteString.Lazy as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.IO as TIO
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import System.Environment (getEnv)

import Data.Char (isDigit)
import Data.Text.Template

main :: IO ()
main = do
  tgt <- getEnv "PANDOC_TARGET"
  let out = output tgt
  let ch  = fileChapter tgt
  txFig ch out

data Output = HTML | LATEX deriving (Eq)

instance Show Output where
  show HTML  = "html"
  show LATEX = "latex"

output :: FilePath -> Output
output = extOut . takeExtension
  where
    extOut ".html" = HTML
    extOut ".pdf"  = LATEX
    extOut s       = error $ "Figures : unknown target: " ++ s

fileChapter :: FilePath -> Maybe Int
fileChapter = toInt . takeWhile isDigit . takeFileName
  where
    toInt "" = Nothing
    toInt xs = Just $ read xs

txFig :: Maybe Int -> Output -> IO ()
txFig ch HTML  = txFigures ch HTML  "" "assets/templates/figHtml.template"
txFig ch LATEX = txFigures ch LATEX "" "assets/templates/figLatex.template"

txFigures :: Maybe Int -> Output -> FilePath -> FilePath -> IO ()
txFigures ch tgt prefix templateF
  = do r    <- newIORef emptyInfo
       tplt <- TIO.readFile templateF
       resetChapter tgt ch r
       toJSONFilter (tx tgt (T.pack prefix) tplt r)

resetChapter :: Output -> Maybe Int -> IORef Info -> IO ()
resetChapter LATEX _        _ = return ()
resetChapter HTML  Nothing  _ = return ()
resetChapter HTML  (Just n) r = replicateM_ (n-1) (newChapter r)

tx :: (Show a) => a -> T.Text -> T.Text -> IORef Info -> Block -> IO Block
tx tgt prefix t r b0
  = do b1    <- txBlock tgt prefix t r b0
       txLink r b1

txLink   :: (Walkable Inline b) => IORef Info -> b -> IO b
txLink r = walkM (reLink r)

reLink   :: IORef Info -> Inline -> IO Inline
-- reLink r (Link attr [Str "auto"] tgt@('#':i,_))
--   = do n <- getCount r i
--        return $ Link attr [Str (tshow n)] tgt

reLink _ i
  = return i

txBlock :: (Show a) => a -> T.Text -> T.Text -> IORef Info -> Block -> IO Block
txBlock _   _      _ r z@(Header 1 _ _)
  = newChapter r >> return z

txBlock tgt prefix t r (Div (i, [cls], kvs) _)
  | isFigure cls
  = makeFigure tgt prefix t r i cls kvs

txBlock _ _ _ _ z
  = return z -- $ trace ("IAMTHIS:" ++ show z) z

isFigure   :: T.Text -> Bool
isFigure s = s `elem` ["figure", "marginfigure"]

makeFigure :: (Show a) => a -> T.Text -> T.Text -> IORef Info -> T.Text -> T.Text -> [(T.Text, T.Text)] -> IO Block
makeFigure tgt prefix t r i cls kvs
  = RawBlock (Format $ tshow tgt) . pad prefix t i cls kvs <$> getCount r i

tshow :: (Show a) => a -> T.Text
tshow = T.pack . show 

pad :: Show a => T.Text -> T.Text -> T.Text -> T.Text -> [(T.Text, T.Text)] -> a -> T.Text
pad prefix tplt i cls kvs n
  = {- trace ("PAD" ++ show res) $ -} res
  where
    res = {- L.unpack $ -} L.toStrict $ substitute tplt ctx
    ctx          :: T.Text -> T.Text
    ctx "class"  = cls
    ctx "label"  = i
    ctx "number" = tshow n
    ctx "file"   = T.append prefix  (get "file" kvs)
    ctx str      = get str kvs

get :: T.Text -> [(T.Text, T.Text)] -> T.Text
get k kvs =   fromMaybe (error $ "Cannot find: " ++ T.unpack k )
            $ lookup k kvs

----------------------------------------------

data Info = Info { chapter :: Int
                 , count   :: Int
                 , label   :: M.Map T.Text Int
                 }
            deriving (Show)

data Ref  = Ref Int Int

instance Show Ref where
  show (Ref i j) = show i ++ "." ++ show j

emptyInfo :: Info
emptyInfo = Info 0 1 M.empty

getCount :: IORef Info -> T.Text -> IO Ref
getCount r idt
  = do info <- readIORef r
       let m  = label info
       let c  = chapter info
       let i  = count info
       let n  = M.findWithDefault i idt m
       let i' = if i == n then i + 1 else i
       let l  = Ref c n
       writeIORef r (info {count = i', label = M.insert idt n m})
       return l -- $ trace ("GETCOUNT: " ++ show l) l

newChapter :: IORef Info -> IO ()
newChapter r
  = do info <- readIORef r
       writeIORef r (info {count = 1, chapter = 1 + chapter info})
