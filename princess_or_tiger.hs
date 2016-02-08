{- Author:     Jeff Newbern
   Maintainer: Jeff Newbern <jnewbern@nomaware.com>
   Time-stamp: <Mon Aug 18 15:36:26 2003>
   License:    GPL
-}

{- DESCRIPTION

Example 24 - Using the StateT monad transformer
             with the List monad to achieve non-deterministic
             stateful computations

Usage: Compile the code and run it.  It will print
       a solution to the following logic puzzle:
       
       An anthropologist is studying a tribe in which men always
       tell the truth and women always alternate between truth
       and lies (they never tell two truths or two lies in a row).
       One day he meets two parents with a child and he asks the
       child if it is a boy or a girl.  The child responds, but
       the anthropologist doesn't understand the language, so he
       asks the parents.  The first parent says "The child said
       it was a boy".  The second parent says "The child is a
       girl.  The child lied."  What is the sex of 
       the child, and which parent is the mother and which is
       the father?

       This puzzle appeared in:
       J.A.H. Hunter, Mathematical Brain-Teasers, Dover Publications 1976
       
Try: ./ex24
-}

--import IO
--import Monad
--import System
import Data.Maybe
import Data.List (permutations)
import Control.Monad.State

-- First, we develop a language to express logic problems
type Var   = String
type Value = String
data Predicate = Is    Var Value            -- var has specific value
               | Equal Var Var              -- vars have same (unspecified) value
               | And   Predicate Predicate  -- both are true
               | Or    Predicate Predicate  -- at least one is true
               | Not   Predicate            -- it is not true
  deriving (Eq, Show)

type Variables = [(Var,Value)]

-- test for a variable NOT equaling a value
isNot :: Var -> Value -> Predicate
isNot var value = Not (Is var value)

-- if a is true, then b must also be true
implies :: Predicate -> Predicate -> Predicate
implies a b = Not (a `And` (Not b))

-- exclusive or
orElse :: Predicate -> Predicate -> Predicate
orElse a b = (a `And` (Not b)) `Or` ((Not a) `And` b)

-- Check a predicate with the given variable bindings.
-- An unbound variable causes a Nothing return value.
check :: Predicate -> Variables -> Maybe Bool
check (Is var value) vars = do val <- lookup var vars
                               return (val == value)
check (Equal v1 v2)  vars = do val1 <- lookup v1 vars
                               val2 <- lookup v2 vars
                               return (val1 == val2)
check (And p1 p2)    vars = liftM2 (&&) (check p1 vars) (check p2 vars)
check (Or  p1 p2)    vars = liftM2 (||) (check p1 vars) (check p2 vars)
check (Not p)        vars = liftM (not) (check p vars)

-- this is the type of our logic problem
data ProblemState = PS {vars::Variables, constraints::[Predicate]}

-- this is our monad type for non-determinstic computations with state
type NDS a = StateT ProblemState [] a

-- lookup a variable
getVar :: Var -> NDS (Maybe Value)
getVar v = do vs <- gets vars
              return $ lookup v vs

-- set a variable
setVar :: Var -> Value -> NDS ()
setVar v x = do st <- get
                vs' <- return $ filter ((v/=).fst) (vars st)
                put $ st {vars=(v,x):vs'}

-- Check if the variable assignments satisfy all of the predicates.
-- The partial argument determines the value used when a predicate returns
-- Nothing because some variable it uses is not set.  Setting this to True
-- allows us to accept partial solutions, then we can use a value of
-- False at the end to signify that all solutions should be complete.
isConsistent :: Bool -> NDS Bool
isConsistent partial = do cs <- gets constraints
                          vs <- gets vars
                          let results = map (\p->check p vs) cs
                          return $ and (map (maybe partial id) results)

-- Return only the variable bindings that are complete consistent solutions.
getFinalVars :: NDS Variables
getFinalVars = do c <- isConsistent False
                  guard c
                  gets vars

-- Get the first solution to the problem, by evaluating the solver computation with
-- an initial problem state and then returning the first solution in the result list,
-- or Nothing if there was no solution.
getSolution :: NDS a -> ProblemState -> Maybe a
getSolution c i = listToMaybe (evalStateT c i)

-- Get a list of all possible solutions to the problem by evaluating the solver
-- computation with an initial problem state.
getAllSolutions :: NDS a -> ProblemState -> [a]
getAllSolutions c i = evalStateT c i

-- now we add some predicate combinators specific to the logic problem

-- if a male says something, it must be true
said :: Var -> Predicate -> Predicate
said v p = And ((v `Is` "true") `implies` p)
               ((v `Is` "false") `implies` (Not p))

-- if a male says two things, they must be true
-- if a female says two things, one must be true and one must be false
saidBoth :: Var -> Predicate -> Predicate -> Predicate
saidBoth v p1 p2 = And ((v `Is` "male") `implies` (p1 `And` p2))
                       ((v `Is` "female") `implies` (p1 `orElse` p2))

-- lying is saying something is true when it isn't or saying something isn't true when it is
lied :: Var -> Predicate -> Predicate
lied v p = ((v `said` p) `And` (Not p)) `orElse` ((v `said` (Not p)) `And` p)

-- Test consistency over all allowed settings of the variable.
tryAllValuesTable :: Var -> NDS ()
tryAllValuesTable var = 
    do (setVar var "true") `mplus` (setVar var "false")
       c <- isConsistent True
       guard c

-- Test consistency over all allowed settings of the variable.
tryAllValuesRoom :: Var -> NDS ()
tryAllValuesRoom var = 
    do (setVar var "lady") `mplus` (setVar var "tiger")
       c <- isConsistent True
       guard c

-- Define the problem, try all of the variable assignments and print a solution.

--- Day One ---
tryTwoRoomsAndTables :: NDS Variables
tryTwoRoomsAndTables =
    do tryAllValuesTable "t2"
       tryAllValuesTable "t1"
       tryAllValuesRoom "r2"
       tryAllValuesRoom "r1"
       getFinalVars
       
ex1 :: IO ()
ex1 = do 
          let variables   = []
              constraints = [ "t1" `said` And ("r1" `Is` "lady") ("r2" `Is` "tiger")
                            , "t2" `said` And (("r1" `Is` "lady") `orElse` ("r2" `Is` "lady"))
                                              (("r1" `Is` "tiger") `orElse` ("r2" `Is` "tiger"))
                            , Not (Equal "t1" "t2")
                            ]                             
              problem = PS variables constraints
          print $ tryTwoRoomsAndTables `getAllSolutions` problem
                                               
ex2 :: IO ()
ex2 = do 
          let variables   = []
              constraints = [ "t1" `said` Or ("r1" `Is` "lady") ("r2" `Is` "lady")
                            , "t2" `said` ("r1" `Is` "tiger")
                            , Equal "t1" "t2"
                            ]                             
              problem = PS variables constraints
          print $ tryTwoRoomsAndTables `getAllSolutions` problem
                                               
ex3 :: IO ()
ex3 = do 
          let variables   = []
              constraints = [ "t1" `said` Or ("r1" `Is` "tiger") ("r2" `Is` "lady")
                            , "t2" `said` ("r1" `Is` "lady")
                            , Equal "t1" "t2"
                            ]                             
              problem = PS variables constraints
          print $  tryTwoRoomsAndTables `getAllSolutions` problem

--- Day Two ---

room1Constr :: Predicate
room1Constr = And (("r1" `Is` "lady") `implies` ("t1" `Is` "true"))
                  (("r1" `Is` "tiger") `implies` ("t1" `Is` "false"))

room2Constr :: Predicate
room2Constr = And (("r2" `Is` "tiger") `implies` ("t2" `Is` "true"))
                  (("r2" `Is` "lady") `implies` ("t2" `Is` "false"))
                  
dayTwoConstr = [room1Constr, room2Constr] :: [Predicate]
                  
ex4 :: IO ()
ex4 = do 
          let variables   = []
              constraints = [ "t1" `said` And ("r1" `Is` "lady") ("r2" `Is` "lady")
                            , "t2" `said` And ("r1" `Is` "lady") ("r2" `Is` "lady")
                            ]                             
              problem = PS variables (dayTwoConstr ++ constraints)
          print $  tryTwoRoomsAndTables `getAllSolutions` problem

ex5 :: IO ()
ex5 = do 
          let variables   = []
              constraints = [ "t1" `said` Or ("r1" `Is` "lady") ("r2" `Is` "lady")
                            , "t2" `said` ("r1" `Is` "lady")
                            ]                             
              problem = PS variables (dayTwoConstr ++ constraints)
          print $  tryTwoRoomsAndTables `getAllSolutions` problem

ex6 :: IO ()
ex6 = do 
          let variables   = []
              constraints = [ "t1" `said` Equal "r1" "r2"
                            , "t2" `said` ("r1" `Is` "lady")
                            ]                             
              problem = PS variables (dayTwoConstr ++ constraints)
          print $  tryTwoRoomsAndTables `getAllSolutions` problem

ex7 :: IO ()
ex7 = do 
          let variables   = []
              constraints = [ "t1" `said` Not (Equal "r1" "r2")
                            , "t2" `said` ("r1" `Is` "lady")
                            ]                             
              problem = PS variables (dayTwoConstr ++ constraints)
          print $  tryTwoRoomsAndTables `getAllSolutions` problem

ex8 :: IO ()
ex8 = do 
          let variables   = [] 
              constraints = [ Or (And ("t1" `said` ("r1" `Is` "tiger"))
                                      ("t2" `said` (And ("r1" `Is` "tiger")
                                                        ("r2" `Is` "tiger"))))
                                 (And ("t2" `said` ("r2" `Is` "tiger"))
                                      ("t1" `said` (And ("r1" `Is` "tiger")
                                                        ("r2" `Is` "tiger"))))
                            ]                             
              problem = PS variables (dayTwoConstr ++ constraints)
          print $  tryTwoRoomsAndTables `getAllSolutions` problem


--- Day Three ---

--- all lists with only `k` `s` in each
onlyK :: Int -> Int -> String -> String -> [[String]]
onlyK k n s s1 = permutations $ replicate k s ++ replicate (n - k) s1

-- Test consistency over all allowed settings of the variable.
tryAll3TwoValuesRoom :: Var -> NDS ()
tryAll3TwoValuesRoom var = 
    do (setVar var "lady") `mplus` (setVar var "tiger")
       c <- isConsistent True
       guard c

tryAll3ThreeValuesRoom :: Var -> NDS ()
tryAll3ThreeValuesRoom var = 
    do (setVar var "lady") `mplus` (setVar var "tiger") `mplus` (setVar var "nothing")
       c <- isConsistent True
       guard c

tryThreeRoomsAndTables :: NDS Variables
tryThreeRoomsAndTables =
    do mapM_ tryAllValuesTable ["t3", "t2", "t1"]
       mapM_ tryAll3TwoValuesRoom ["r3", "r2", "r1"]
       getFinalVars
       
ex9 :: IO ()
ex9 = do 
          let variables   = []
              constraints = [ foldl1 orElse $ map (`Is` "true") ["t1", "t2", "t3"]
                            , foldl1 orElse $ map (`Is` "lady") ["r1", "r2", "r3"]
                            , "t1" `said` ("r1" `Is` "tiger")
                            , "t2" `said` ("r2" `Is` "lady")
                            , "t3" `said` ("r2" `Is` "tiger") 
                            ]                             
              problem = PS variables constraints
          print $  tryThreeRoomsAndTables `getAllSolutions` problem

testOrElse n = 
  let 
    variables = map (('r':).show) [1 .. n]
    constraints =
      [foldl1 orElse $ map (`Is` "lady") variables]
    tryThreeRooms =
      do
        mapM tryAllValuesRoom variables
        getFinalVars
    problem = PS [] constraints
  in
    tryThreeRooms `getAllSolutions` problem

-- END OF FILE
