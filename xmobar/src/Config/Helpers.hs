{-# LANGUAGE PostfixOperators    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Config.Helpers where

inIconFont :: String -> String
inIconFont = wrap "<fn=1>" "</fn>"

inColor :: String -> String -> String
inColor color = wrap ("<fc=" <> color <> ">") "</fc>"

inAltIconFont :: String -> String
inAltIconFont = wrap "<fn=2>" "</fn>"

wrap
  :: String  -- ^ left delimiter
  -> String  -- ^ right delimiter
  -> String  -- ^ output string
  -> String
wrap _ _ "" = ""
wrap l r m  = l <> m <> r

configDir :: String
configDir = "/home/mzanic/.config"

seconds, minutes :: Int -> Int
seconds = (* 10)
minutes = (60 *) . seconds

colorSeparator :: String -> String
colorSeparator c = inColor myppSepColor $ "  " <> c <> "  "
  where
    myppSepColor :: String = "#586E75"

screenLog :: Int -> String
screenLog n = "_XMONAD_LOG_" <> show n
