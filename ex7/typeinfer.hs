-- Σταματελόπουλος Νικόλαος
-- Α.Μ. : 03116138

import Data.Char
import System.IO
import Text.Read
import Data.Map as Map hiding (take)
import Prelude

data Type  =  Tvar Int | Tfun Type Type                        deriving Eq
data Expr  =  Evar String | Eabs String Expr | Eapp Expr Expr  deriving Eq

data TypedExpr = TypedEvar Type | TypedEabs TypedExpr Type | TypedEapp TypedExpr TypedExpr Type deriving Eq

-- Pretty printing of expressions

always = True     -- False omits parentheses whenever possible

instance Show Expr where
  showsPrec p (Evar x) = (x ++)
  showsPrec p (Eabs x e) =
    showParen (always || p > 0) ((("\\" ++ x ++ ". ") ++) . showsPrec 0 e)
  showsPrec p (Eapp e1 e2) =
    showParen (always || p > 1) (showsPrec 1 e1 . (" " ++) . showsPrec 2 e2)

-- Parsing of expressions

instance Read Expr where
  readPrec = (do Ident x <- lexP
                 return (Evar x)) <++
             (do Punc "(" <- lexP
                 Punc "\\" <- lexP
                 Ident x <- lexP
                 Symbol "." <- lexP
                 e <- readPrec
                 Punc ")" <- lexP
                 return (Eabs x e)) <++
             (do Punc "(" <- lexP
                 e1 <- readPrec
                 e2 <- readPrec
                 Punc ")" <- lexP
                 return (Eapp e1 e2))

-- Pretty printing of types

instance Show Type where
  showsPrec p (Tvar alpha) = ("@" ++) . showsPrec 0 alpha
  showsPrec p (Tfun sigma tau) =
    showParen (p > 0) (showsPrec 1 sigma . (" -> " ++) . showsPrec 0 tau)

instance Show TypedExpr where
  showsPrec p (TypedEvar t) = (show t ++)
  showsPrec p (TypedEabs e t) =
    showParen (always || p > 0) ((show t ++))
  showsPrec p (TypedEapp e1 e2 _) =
    showParen (always || p > 1) (showsPrec 1 e1 . (" " ++) . showsPrec 2 e2)

-- just to get the type of a typed expression
getType :: TypedExpr -> Type
getType x =
  case x of
    TypedEvar t -> t
    TypedEabs _ t -> t
    TypedEapp _ _ t -> t


-- Add type annotations to expressions
annotate :: Expr -> Int -> Map String Int -> (TypedExpr, Int)
annotate expr id map =  -- useing a map to keep track of variables
  case expr of
    Evar var ->  -- var: @... something already seen (can't be unbound)
      case (Map.lookup var map) of
        Just t -> (TypedEvar (Tvar t), id)
        Nothing -> error "type error"
    Eabs var exp ->  -- (exp: @id -> @id')
      (TypedEabs  exp' (Tfun (Tvar id) (getType exp')), id')
      where
        (exp', id') = annotate exp (id+1) (Map.insert var id map)
    Eapp exp1 exp2 ->  -- (exp1: @id', exp2 @id''): @id
      (TypedEapp exp1' exp2' (Tvar id), id'')
      where
        (exp1', id') = annotate exp1 (id+1) map
        (exp2', id'') = annotate exp2 id' map

-- Find the constraints for annotated expression
findConstraints :: [TypedExpr] -> [(Type, Type)] -> [(Type, Type)]
findConstraints exprList constraints =
  case exprList of
    [] -> constraints
    ((TypedEvar _) : ls) -> findConstraints ls constraints
    ((TypedEabs exp _) : ls) -> findConstraints (exp : ls) constraints
    ((TypedEapp exp1 exp2 t) : ls) -> findConstraints (exp1 : exp2 : ls) ((t1, Tfun t2 t) : constraints)
      where
        t1 = getType exp1
        t2 = getType exp2

-- check if type of first arguments appears in second argument
notAppearsInType :: Type -> Type -> Bool
notAppearsInType t1 t2@(Tvar _) = t1 /= t2
notAppearsInType t1 t2@(Tfun t21 t22) = t1 /= t2 && notAppearsInType t1 t21 && notAppearsInType t1 t22

-- substitute type in type expression
substituteType :: Type -> Type -> Type -> Type
substituteType tOld tNew t@(Tvar _)
  | tOld == t = tNew
  | otherwise  = t
substituteType tOld tNew (Tfun t1 t2) = Tfun t1' t2'
  where 
    t1' = substituteType tOld tNew t1
    t2' = substituteType tOld tNew t2

-- unifier (finding solution based on types and the constraints on them)
unify :: [(Type, Type)] -> Type -> Maybe Type
unify [] t = Just  t
unify ((t1,t2):c) t | t1 == t2 = unify c t
unify ((t1@(Tvar id),t2):c) t | notAppearsInType t1 t2 = unify c' t'
    where
      c' = Prelude.map (\(x,y) -> (substituteType t1 t2 x, substituteType t1 t2 y)) c
      t' = substituteType t1 t2 t
unify ((t1,t2@(Tvar id)):c) t | notAppearsInType t2 t1 = unify c' t'
    where
      c' = Prelude.map (\(x,y) -> (substituteType t2 t1 x, substituteType t2 t1 y)) c
      t' = substituteType t2 t1 t
unify (((Tfun t11 t12),(Tfun t21 t22)):c) t = unify ((t11,t21):(t12,t22):c) t
unify _ _ = Nothing

-- just to find the 'lexicographically' smallest type
-- just a 'DFS' traversal of the expression,
-- substituting the types with the lowest possible
-- while keeping track of previous substitutions using a map
smallestExpr :: Type -> Int -> (Map Int Int) -> (Type, Int, (Map Int Int))
smallestExpr (Tvar id) nextId map =
  case (Map.lookup id map) of
    Just id' -> (Tvar id', nextId, map)
    Nothing -> (Tvar nextId, nextId+1, (Map.insert id nextId map))
smallestExpr (Tfun t1 t2) nextId map = ((Tfun t1' t2'), nextId'', map'')
  where
    (t1', nextId', map') = smallestExpr t1 nextId map
    (t2', nextId'', map'') = smallestExpr t2 nextId' map'

-- Main program

readOne  =  do  s <- getLine
                let e = read s :: Expr
                let (tmp, _) = annotate e 0 Map.empty
                let c = findConstraints [tmp] []
                let inferedType = unify c (getType tmp)
                --putStrLn ("Parsed: " ++ show e)
                --putStrLn ("Annotated: " ++ show tmp)
                --putStrLn ("Constraints found: " ++ show c)
                --putStrLn ("Infered type: " ++ show inferedType)
                case inferedType of
                  Just t ->
                    let (t', _, _) = smallestExpr t 0 Map.empty 
                    in putStrLn (show t')
                  Nothing ->
                    putStrLn ("type error")
                    

count n m  =  sequence $ take n $ repeat m

main     =  do  n <- readLn
                count n readOne
