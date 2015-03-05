import Distribution.Simple
import Distribution.Simple.Program

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
       {
         -- cabal-install is used during testing to run
         -- and verify haddock documentation.
         hookedPrograms = [ simpleProgram "cabal" ]
       }
