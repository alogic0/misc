module Baker where
import Data.List (sortBy)

type Ingredient = String
type Amount     = Int
type Recipe     = [(Ingredient, Amount)]
type Storage    = [(Ingredient, Amount)]

cakes :: Recipe -> Storage -> Int
cakes recipe storage =
  let listOfIngr = (fst.unzip) recipe
      storageI = filter (\(i,_) -> i `elem` listOfIngr) storage
      sortIng = sortBy (\(i1,_) (i2,_) -> compare i1 i2)
      storageVec = sortIng storageI
      recipeVec = sortIng recipe
