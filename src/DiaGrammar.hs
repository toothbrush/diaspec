module DiaGrammar where

-- possible types for our components
data Type = Bool
          | Int
          | String
          | Picture
            deriving (Eq, Show)

type Specification = [Declaration]

data Declaration = Source String Type
                 | Action String Type
                 | Context String Type ContextInteraction
                 | Controller String ControllerInteraction
                   deriving (Eq, Show)

data ContextInteraction = WhenRequired Get
                        | WhenProvided String Get Publish
                          deriving (Eq, Show)

data ControllerInteraction = WhenContext String String
                             deriving (Eq, Show)
                                     
type Get = Maybe String

data Publish = AlwaysPublish
             | MaybePublish
               deriving (Eq, Show)
