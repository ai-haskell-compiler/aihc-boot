{-# LANGUAGE BangPatterns #-}
module Data.Set.Internal (Set(..), Size, empty, singleton, insert, member, size) where {
import GHC.Exts (lazy);
import Utils.Containers.Internal.PtrEquality (ptrEq);
data Set a = Bin !Size !a !(Set a) !(Set a) | Tip;
type Size = Int;
size :: Set a -> Int;
size Tip = 0;
size (Bin sz _ _ _) = sz;
member :: Ord a => a -> Set a -> Bool;
member = go where {
  go !_ Tip = False;
  go x (Bin _ y l r) = case compare x y of {
  LT -> go x l;
  GT -> go x r;
  EQ -> True}};
empty :: Set a;
empty = Tip;
singleton :: a -> Set a;
singleton x = Bin 1 x Tip Tip;
insert :: Ord a => a -> Set a -> Set a;
insert x0 = go x0 x0 where {
  go :: Ord a => a -> a -> Set a -> Set a;
  go orig !_ Tip = singleton (lazy orig);
  go orig !x t@(Bin sz y l r) = case compare x y of {
  LT | l' `ptrEq` l -> t | otherwise -> balanceL y l' r where {
  !l' = go orig x l};
  GT | r' `ptrEq` r -> t | otherwise -> balanceR y l r' where {
  !r' = go orig x r};
  EQ | lazy orig `seq` (orig `ptrEq` y) -> t | otherwise -> Bin sz (lazy orig) l r}};
delta, ratio :: Int;
delta = 3;
ratio = 2;
balanceL :: a -> Set a -> Set a -> Set a;
balanceL x l r = case r of {
  Tip -> case l of {
  Tip -> Bin 1 x Tip Tip;
  (Bin _ _ Tip Tip) -> Bin 2 x l Tip;
  (Bin _ lx Tip (Bin _ lrx _ _)) -> Bin 3 lrx (Bin 1 lx Tip Tip) (Bin 1 x Tip Tip);
  (Bin _ lx ll@(Bin _ _ _ _) Tip) -> Bin 3 lx ll (Bin 1 x Tip Tip);
  (Bin ls lx ll@(Bin lls _ _ _) lr@(Bin lrs lrx lrl lrr)) | lrs < ratio * lls -> Bin (1 + ls) lx ll (Bin (1 + lrs) x lr Tip) | otherwise -> Bin (1 + ls) lrx (Bin (1 + lls + size lrl) lx ll lrl) (Bin (1 + size lrr) x lrr Tip)};
  (Bin rs _ _ _) -> case l of {
  Tip -> Bin (1 + rs) x Tip r;
  (Bin ls lx ll lr) | ls > delta * rs -> case (ll, lr) of {
  (Bin lls _ _ _, Bin lrs lrx lrl lrr) | lrs < ratio * lls -> Bin (1 + ls + rs) lx ll (Bin (1 + rs + lrs) x lr r) | otherwise -> Bin (1 + ls + rs) lrx (Bin (1 + lls + size lrl) lx ll lrl) (Bin (1 + rs + size lrr) x lrr r);
  (_, _) -> error "Failure in Data.Set.balanceL"} | otherwise -> Bin (1 + ls + rs) x l r}};
balanceR :: a -> Set a -> Set a -> Set a;
balanceR x l r = case l of {
  Tip -> case r of {
  Tip -> Bin 1 x Tip Tip;
  (Bin _ _ Tip Tip) -> Bin 2 x Tip r;
  (Bin _ rx Tip rr@(Bin _ _ _ _)) -> Bin 3 rx (Bin 1 x Tip Tip) rr;
  (Bin _ rx (Bin _ rlx _ _) Tip) -> Bin 3 rlx (Bin 1 x Tip Tip) (Bin 1 rx Tip Tip);
  (Bin rs rx rl@(Bin rls rlx rll rlr) rr@(Bin rrs _ _ _)) | rls < ratio * rrs -> Bin (1 + rs) rx (Bin (1 + rls) x Tip rl) rr | otherwise -> Bin (1 + rs) rlx (Bin (1 + size rll) x Tip rll) (Bin (1 + rrs + size rlr) rx rlr rr)};
  (Bin ls _ _ _) -> case r of {
  Tip -> Bin (1 + ls) x l Tip;
  (Bin rs rx rl rr) | rs > delta * ls -> case (rl, rr) of {
  (Bin rls rlx rll rlr, Bin rrs _ _ _) | rls < ratio * rrs -> Bin (1 + ls + rs) rx (Bin (1 + ls + rls) x l rl) rr | otherwise -> Bin (1 + ls + rs) rlx (Bin (1 + ls + size rll) x l rll) (Bin (1 + rrs + size rlr) rx rlr rr);
  (_, _) -> error "Failure in Data.Set.balanceR"} | otherwise -> Bin (1 + ls + rs) x l r}}
}
