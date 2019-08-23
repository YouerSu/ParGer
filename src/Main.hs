import qualified RuleLexer as L
import qualified RuleParser as P
import qualified CodeGenerator as G
import System.IO

main = do
  pgFileName <- getLine
  file <- readFile pgFileName
  generate file

generate = G.generate . P.parseStream . L.expStream
