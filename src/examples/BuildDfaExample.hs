import Language.Mira.RegExpParser as Parser
import Language.Mira.MinimiseDfa
import Language.Mira.BuildNfa
import Language.Mira.NfaToDfa
import Language.Mira.NfaMisc

import Control.Monad
import System.Environment

main = liftM head getArgs >>= putStr . print_nfa . minimise . make_deterministic . build . Parser.regex . Parser.lexer