{-# LANGUAGE RecordWildCards #-}
                                 -- they make your code clean and clear.
                                 -- Read about this extension here:
                                 -- https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html



module Construction.Internal.Functions
  ( Context (..)        -- make restrictions is good practice. As you can see here,
  , fresh, free, bound  -- we make "public" not all functions, but only Context, fresh, ...
  , reduce, substitute, alpha, beta, eta, findBnf, countApp
  )where

import           Construction.Internal.Types (Name, Term (..))
import           Data.Set                    (Set, delete, empty, insert,
                                              member, notMember, singleton,
                                              union)
import           Data.Text                   (pack)

instance Eq Term where
  (Var lhsVar) == (Var rhsVar) = True
  (App lhsAlgo lshArg) == (App rhsAlgo rhsArg) =  (lhsAlgo == rhsAlgo) && (lshArg == rhsArg)
  l@(Lam lhsVar lhsBody) == r@(Lam rhsVar rhsBody) = lhsBody == (substitute rhsBody rhsVar (Var lhsVar))
  lhs == rhs = (beta lhs) == (beta rhs)

-- Context is just set of names that are in our context.
type Context = Set Name

-- | @fresh@ generates new variable different from every variables in set.
fresh :: Set Name -> Name
fresh conflicts = head . dropWhile (`member` conflicts) $ nameGen -- This is ugly name generator. Make it better.
  where nameGen = [pack $ 'x' : show ind | ind <- [0..] :: [Int]]

-- | @free@ finds all free (Amazing!) variables from given term.
free :: Term -> Set Name
free (Var var)           = singleton var
free (App algo arg)      = free algo `union` free arg
free (Lam variable body) = variable `delete` free body

-- | @bound@ finds all bounded variables from given term.
-- This function uses RecordWildCards.
-- If you like it refactor @free@ function.
bound :: Term -> Set Name
bound Var{}   = empty
bound App{..} = bound algo `union` bound arg
bound Lam{..} = variable `insert` bound body

-- a[n := b] - substiturion
substitute :: Term -> Name -> Term -> Term
substitute v@Var{..} n b | var == n  = b
                         | otherwise = v
substitute App{..} n b = App (substitute algo n b) (substitute arg n b)
substitute l@(Lam variable body) n b | variable `member` (free b) = substitute (alpha l (free b)) n b
                                   | otherwise = if (n == variable) then l else (Lam variable (substitute body n b))

-- | alpha reduction
alpha :: Term -> Set Name -> Term
alpha (Var var) names = if (var `member` names) then Var (fresh names) else (Var var)
alpha (Lam variable body) names = let varName = if (variable `member` names) then fresh names else variable
                            in (Lam varName (alpha (if (variable `member` names) then (substitute body variable (Var varName)) else body) names))
alpha (App algo arg) names = (App (alpha algo names) (alpha arg names))

-- | beta reduction
beta :: Term -> Term
beta v@(Var variable) = v
beta a@(App (Lam variable body) arg) = if (countApp (substitute body variable arg)) > (countApp body) then a else  beta (substitute body variable arg)
beta (App algo arg) = (App  (beta algo) (alpha arg (bound arg)))
beta a@(Lam variable body) = if (countApp (beta body)) >= (countApp body) then a else (Lam variable (beta body))

-- | eta reduction
eta :: Term -> Term
eta v@(Var variable) = v
eta (App algo arg) = (App  (eta algo) (eta arg))
eta a@(Lam variable (App algo (Var x))) = if (variable == x && not (x `member` (free algo))) then (eta algo) else a
eta l@(Lam variable body) = l

-- | reduce term
reduce :: Term -> Term
reduce term = let term' = beta term
              in if term' == term
                 then eta term
                 else reduce term'

countApp :: Term -> Int
countApp (Var var) = 0
countApp (Lam var body) = (countApp body)
countApp (App algo arg) = 1 + (countApp algo) + (countApp arg)

findBnf ::Term -> Bool
findBnf (Var var) = True
findBnf a@(App algo arg) = let count = (countApp a)
                          in if (countApp (beta a)) >= count then False else (findBnf (beta a))
findBnf (Lam variable body) = True
