import Test.QuickCheck

main = do
  quickCheck $ forAll setCases prop_headIsMember
  quickCheck $ forAll setCases prop_otherIsNotMember
  quickCheck $ forAll mapCases prop_getHead
  quickCheck $ forAll mapCases prop_getOther

-- PART a
type Key = String
data Tree = Leaf | Tree Tree Key Tree
  deriving (Show)

empty :: Tree
empty = Leaf

insert :: Key -> Tree -> Tree
insert k' Leaf = Tree Leaf k' Leaf
insert k' (Tree l k r)
  | k' < k = Tree (insert k' l) k r
  | k' > k = Tree l k (insert k' r)
  | otherwise = Tree l k r

member :: Key -> Tree -> Bool
member k' Leaf = False
member k' (Tree l k r) = memberer k (Tree l k r)
  where memberer :: Key -> Tree -> Bool
        memberer k'' Leaf = k' == k''
        memberer k'' (Tree l k r)
          | k' < k = memberer k'' l
          | otherwise = memberer k r


setCases :: Gen (Key, [Key], Tree)
setCases =
  do ks <- listOf1 $ listOf1 $ choose ('a', 'z')
     k' <- listOf1 $ choose ('a', 'z')
     return $ (k', ks, foldr insert Leaf ks)

prop_headIsMember :: (Key, [Key], Tree) -> Bool
prop_headIsMember (_, k : _, t) = member k t

prop_otherIsNotMember :: (Key, [Key], Tree) -> Bool
prop_otherIsNotMember (k', ks, t) = elem k' ks || not (member k' t)


-- PART b
data VTree a = VLeaf | VTree (VTree a) Key a (VTree a)
  deriving (Show)

put :: Key -> a -> VTree a -> VTree a
put k' v' VLeaf = VTree VLeaf k' v' VLeaf
put k' v' (VTree l k v r)
  | k' < k = VTree (put k' v' l) k v r
  | k' > k = VTree l k v (put k' v' r)
  | otherwise = VTree l k v' r

get :: Key -> VTree a -> Maybe a
get k' VLeaf = Nothing
get k' (VTree l k v r) = getter k v (VTree l k v r)
  where getter :: Key -> a -> VTree a -> Maybe a
        getter k'' v'' VLeaf = if k' == k'' then Just v'' else Nothing
        getter k'' v'' (VTree l k v r)
          | k' < k = getter k'' v'' l
          | otherwise = getter k v r

mapCases :: Gen (Key, [(Key, Int)], VTree Int)
mapCases =
  do ks <- listOf1 $ listOf1 $ choose ('a', 'z')
     let assocs = zip ks $ map length ks
     k' <- listOf1 $ choose ('a', 'z')
     return (k', assocs, foldr (uncurry put) VLeaf assocs)

prop_getHead :: (Key, [(Key, Int)], VTree Int) -> Bool
prop_getHead (_, (k, v) : _, t) = get k t == Just v

prop_getOther :: (Key, [(Key, Int)], VTree Int) -> Bool
prop_getOther (k', assocs, t) = elem k' (map fst assocs) || get k' t == Nothing

-- PART c
--               t                 a
--              /                   \
--             s                     b
--            /                       \
--           p                         c
--          /                           \
--         i                             d
--        /                               \
--       f                                 e
--      /                                   \
--     b                                     f
--                                            \
--                                             g
--                                              \
--                                               h
--                                                \
--                                                 i

-- Part d
-- Red-black trees as described in Purely Functional Data Structures should
-- work.