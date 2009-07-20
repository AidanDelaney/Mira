import Language.Mira.RegExpParser as Parser

import Control.Monad
import System.Environment

main = liftM head getArgs >>= print . Parser.regex . Parser.lexer