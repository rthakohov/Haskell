module Construction.Internal.Types
  ( Name, Term(..)
  ) where

import Data.Text (Text) -- we want to import only Text from Data.Text.
import           Data.Set                    (Set, delete, empty, insert,
                                              member, notMember, singleton,
                                              union)

type Name = Text -- just alias, no more

data Term = Var { var :: Name }                     -- Variables: a, b, ...
          | App { algo :: Term, arg :: Term }       -- Application: M N
          | Lam { variable :: Name, body :: Term }  -- Abstraction: \x. M
  deriving (Show) -- we deriving some common classes like Show.
                           -- With this deriving you can use function "show"
                           -- to print your term.