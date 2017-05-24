{-# LANGUAGE RankNTypes #-}

module Main where

import Protolude
import Control.Monad.Free

-- | Free Monad + Interpreter Pattern
--
-- The type 'Http' below captures actions at type level.
-- Needs to be a Functor for composing the completion.
--
-- 'Free Http Response' then represents a type level list
-- with actions from 'Http' and their result 'Response'.
--
-- We can interpret a list of actions with 'runIO', which
-- executes all actions in the IO monad.
--
-- Other interpreters are possible, e.g. debug interpreter.
--
-- 'submitForm' is an example how to construct pure actions
-- which can then later be evaluated (e.g. in IO) or not.
--

data Url = Url
  { _host :: Text
  , _port :: Int }
  deriving (Show)

data HttpError = HttpError
  { _code :: Int }
  deriving (Show)

data HttpOk = HttpOk
  { _body :: Text }
  deriving (Show)

type HttpResponse = Either HttpError HttpOk


data Http next
  = Get  Url (HttpResponse -> next)
  | Post Url (HttpResponse -> next)
  | Done


instance Functor Http where
  fmap f (Get  url next) = Get  url (f . next)
  fmap f (Post url next) = Post url (f . next)
  fmap _ (Done)          = Done


type HttpInteraction = Free Http


httpGet :: Url -> HttpInteraction HttpResponse
httpGet url = liftF $ Get url identity

httpPost :: Url -> HttpInteraction HttpResponse
httpPost url = liftF $ Post url identity


unsafeRunIO :: HttpInteraction a -> IO ()
unsafeRunIO act = case act of
  Free (Get url next) -> do
    putText $ "GET request to " <> show url
    unsafeRunIO $ next (Right $ HttpOk "Website")

  Free (Post url next) -> do
    putText $ "PUT request to " <> show url
    unsafeRunIO $ next (Right $ HttpOk "Submitted")

  Free (Done) -> do
    return ()

  Pure _ -> do
    return ()


runIO :: (forall a. HttpInteraction a) -> IO ()
runIO = unsafeRunIO


submitForm :: Url -> HttpInteraction (Http a)
submitForm url = do
  getRes <- httpGet url

  case getRes of
    Left  (HttpError _) -> return Done
    Right (HttpOk    _) -> do
      postRes <- httpPost url
      case postRes of
        Left  (HttpError _) -> return Done
        Right (HttpOk    _) -> return Done


main :: IO ()
main = do
  unsafeRunIO $ submitForm $ Url "example.com" 80


-- | Questions
--
--   1/ How to mix in error handling in 'submitForm'?
--      Monad Transformers, e.g. 'ExcepT HttpError (HttpInteraction a)' ?
--      Responses are already 'Either's can we chain fmap easily on them?
--
--   2/ What does it mean to compose DSL functors?
--      Sum, Product, Compose
--
--   3/ 
--
