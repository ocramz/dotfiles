import XMonad

main :: IO ()
main = xmonad defaultConfig { terminal = "urxvt" }
