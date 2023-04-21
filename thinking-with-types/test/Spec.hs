import Converter
import Data.Text as T
import Data.Text.IO as T

main :: IO ()
main = T.readFile "src/Lib.hs" >>= T.writeFile "TWT.md" . (Hs `convertTo` Md) (def & _indent ~? "INDENT" &)
