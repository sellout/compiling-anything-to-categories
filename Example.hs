import qualified Categorifier.Categorify as Categorify

Categorify.function 'doTheThing ''Syn []

main :: IO ()
main = writeToFile $ runSyn wrap_doTheThing
