-- | Homework 8

module Party where

import           Data.List
import           Data.Tree
import           Employee
import           Text.Printf

--Exercise 1
glCons :: Employee -> GuestList -> GuestList
glCons employee (GL employees_list fun) =
  GL (employee : employees_list) (fun + empFun employee)

instance Semigroup GuestList where
  (GL left_list left_fun) <> (GL right_list right_fun) =
    GL (left_list ++ right_list) (left_fun + right_fun)

instance Monoid GuestList where
  mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun left@(GL _ l_fun) right@(GL _ r_fun) | l_fun > r_fun = left
                                             | otherwise     = right

-- Exercise 2
treeFold :: (b -> a -> b) -> b -> Tree a -> b
treeFold f z (Node { rootLabel = root, subForest = children }) =
  foldl (treeFold f) (f z root) children

-- Exercise 3
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel employee subtree_guest_lists =
  let (with_subtree_boss, without_subtree_boss) = unzip subtree_guest_lists
  in  ( glCons employee $ mconcat without_subtree_boss
      , mconcat with_subtree_boss
      )

-- Exercise 4
maxFun :: Tree Employee -> GuestList
maxFun t | fun_with_root > fun_wo_root = with_root
         | otherwise                   = without_root
 where
  maxFun' (Node { rootLabel = employee, subForest = subtrees }) =
    nextLevel employee (map maxFun' subtrees)
  (with_root@(GL _ fun_with_root), without_root@(GL _ fun_wo_root)) = maxFun' t

main :: IO ()
main = do
  file_contents <- readFile "company.txt"
  let (GL employees fun) = maxFun $ read file_contents
  () <- putStrLn $ printf "Total fun: %d" fun
  mapM_ putStrLn (sort $ map empName $ employees)
