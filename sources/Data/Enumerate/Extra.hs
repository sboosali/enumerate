{-# LANGUAGE RankNTypes, LambdaCase #-}
module Data.Enumerate.Extra where

import Control.Monad.Catch

import Control.Arrow ((&&&), (>>>))


{-| @failed = 'throwM' . 'userError'@

-}
failed ::  (MonadThrow m) => String -> m a
failed = throwM . userError

-- | generalize a function that fails with @Nothing@. 
maybe2throw :: (a -> Maybe b) -> (forall m. MonadThrow m => a -> m b) 
maybe2throw f = f >>> \case
 Nothing -> failed "Nothing"
 Just x  -> return x 

-- | generalize a function that fails with @[]@.  
list2throw :: (a -> [b]) -> (forall m. MonadThrow m => a -> m b)
list2throw f = f >>> \case
 []    -> failed "[]"

 (x:_) -> return x

-- | generalize a function that fails with @Left@. 
either2throw :: (a -> Either SomeException b) -> (forall m. MonadThrow m => a -> m b)
either2throw f = f >>> \case
 Left  e -> throwM e
 Right x -> return x 
