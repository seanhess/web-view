{-# LANGUAGE LambdaCase #-}

module Example.Effects.Debug where

import Data.String.Interpolate (i)
import Effectful
import Effectful.Dispatch.Dynamic

data Debug :: Effect where
  Dump :: Show a => String -> a -> Debug m ()

type instance DispatchOf Debug = 'Dynamic

runDebugIO
  :: (IOE :> es)
  => Eff (Debug : es) a
  -> Eff es a
runDebugIO = interpret $ \_ -> \case
  Dump msg a -> do
    liftIO $ putStrLn [i| [#{msg}] #{show a}|]

dump :: (Debug :> es, Show a) => String -> a -> Eff es ()
dump msg a = send $ Dump msg a
