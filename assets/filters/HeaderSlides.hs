{-@ LIQUID "--no-termination" @-}

module HeaderSlides where

import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter txPandoc

txPandoc :: Pandoc -> Pandoc
txPandoc (Pandoc m bs)   = Pandoc m bs''
  where
    bs''                 = preBs ++ [ slideDiv i h bs' | (i, (h, bs')) <- hbss']
    hbss'                = zip [0..] hbss
    (preBs, hbss)        = splitOn isHdr bs
    isHdr (Header n _ _) = n <= 2
    isHdr _              = False

slideDiv :: Int -> Block -> [Block] -> Block
slideDiv i h bs = Div attr (h:bs)
  where
    attr        = ("slide-" ++ show i, ["slide"], [])

splitOn :: (a -> Bool) -> [a] -> ([a], [(a, [a])])
splitOn f xs    = (pre, splitOn' f rest)
  where
    (pre, rest) = break f xs

splitOn' :: (a -> Bool) -> [a] -> [(a, [a])]
splitOn' _ []     = []
splitOn' f (x:xs) = (x, pre) : splitOn' f rest
  where
    (pre, rest)   = break f xs
