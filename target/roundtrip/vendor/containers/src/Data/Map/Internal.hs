{-# LANGUAGE BangPatterns #-}
module Data.Map.Internal (Map(..), Size, empty, singleton, delete, lookup, member, size, foldrWithKey, bin, balanceL, balanceR, glue, link, insertMax, insertMin) where {
import Prelude hiding (lookup);
import Utils.Containers.Internal.PtrEquality (ptrEq);
data Map k a = Bin !Size !k a !(Map k a) !(Map k a) | Tip;
type Size = Int;
size :: Map k a -> Int;
size Tip = 0;
size (Bin sz _ _ _ _) = sz;
lookup :: Ord k => k -> Map k a -> Maybe a;
lookup = go where {
  go !_ Tip = Nothing;
  go k (Bin _ kx x l r) = case compare k kx of {
  LT -> go k l;
  GT -> go k r;
  EQ -> Just x}};
member :: Ord k => k -> Map k a -> Bool;
member = go where {
  go !_ Tip = False;
  go k (Bin _ kx _ l r) = case compare k kx of {
  LT -> go k l;
  GT -> go k r;
  EQ -> True}};
empty :: Map k a;
empty = Tip;
singleton :: k -> a -> Map k a;
singleton k x = Bin 1 k x Tip Tip;
delete :: Ord k => k -> Map k a -> Map k a;
delete = go where {
  go :: Ord k => k -> Map k a -> Map k a;
  go !_ Tip = Tip;
  go k t@(Bin _ kx x l r) = case compare k kx of {
  LT | l' `ptrEq` l -> t | otherwise -> balanceR kx x l' r where {
  !l' = go k l};
  GT | r' `ptrEq` r -> t | otherwise -> balanceL kx x l r' where {
  !r' = go k r};
  EQ -> glue l r}};
foldrWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b;
foldrWithKey f z = go z where {
  go z' Tip = z';
  go z' (Bin _ kx x l r) = go (f kx x (go z' r)) l};
link :: k -> a -> Map k a -> Map k a -> Map k a;
link kx x Tip r = insertMin kx x r;
link kx x l Tip = insertMax kx x l;
link kx x l@(Bin sizeL ky y ly ry) r@(Bin sizeR kz z lz rz) | delta * sizeL < sizeR = balanceL kz z (link kx x l lz) rz | delta * sizeR < sizeL = balanceR ky y ly (link kx x ry r) | otherwise = bin kx x l r;
insertMax, insertMin :: k -> a -> Map k a -> Map k a;
insertMax kx x t = case t of {
  Tip -> singleton kx x;
  Bin _ ky y l r -> balanceR ky y l (insertMax kx x r)};
insertMin kx x t = case t of {
  Tip -> singleton kx x;
  Bin _ ky y l r -> balanceL ky y (insertMin kx x l) r};
glue :: Map k a -> Map k a -> Map k a;
glue Tip r = r;
glue l Tip = l;
glue l@(Bin sl kl xl ll lr) r@(Bin sr kr xr rl rr) | sl > sr = let {
  !(MaxView km m l') = maxViewSure kl xl ll lr} in balanceR km m l' r | otherwise = let {
  !(MinView km m r') = minViewSure kr xr rl rr} in balanceL km m l r';
data MinView k a = MinView !k a !(Map k a);
data MaxView k a = MaxView !k a !(Map k a);
minViewSure :: k -> a -> Map k a -> Map k a -> MinView k a;
minViewSure = go where {
  go k x Tip r = MinView k x r;
  go k x (Bin _ kl xl ll lr) r = case go kl xl ll lr of {
  MinView km xm l' -> MinView km xm (balanceR k x l' r)}};
maxViewSure :: k -> a -> Map k a -> Map k a -> MaxView k a;
maxViewSure = go where {
  go k x l Tip = MaxView k x l;
  go k x l (Bin _ kr xr rl rr) = case go kr xr rl rr of {
  MaxView km xm r' -> MaxView km xm (balanceL k x l r')}};
delta, ratio :: Int;
delta = 3;
ratio = 2;
balanceL :: k -> a -> Map k a -> Map k a -> Map k a;
balanceL k x l r = case r of {
  Tip -> case l of {
  Tip -> Bin 1 k x Tip Tip;
  (Bin _ _ _ Tip Tip) -> Bin 2 k x l Tip;
  (Bin _ lk lx Tip (Bin _ lrk lrx _ _)) -> Bin 3 lrk lrx (Bin 1 lk lx Tip Tip) (Bin 1 k x Tip Tip);
  (Bin _ lk lx ll@(Bin _ _ _ _ _) Tip) -> Bin 3 lk lx ll (Bin 1 k x Tip Tip);
  (Bin ls lk lx ll@(Bin lls _ _ _ _) lr@(Bin lrs lrk lrx lrl lrr)) | lrs < ratio * lls -> Bin (1 + ls) lk lx ll (Bin (1 + lrs) k x lr Tip) | otherwise -> Bin (1 + ls) lrk lrx (Bin (1 + lls + size lrl) lk lx ll lrl) (Bin (1 + size lrr) k x lrr Tip)};
  (Bin rs _ _ _ _) -> case l of {
  Tip -> Bin (1 + rs) k x Tip r;
  (Bin ls lk lx ll lr) | ls > delta * rs -> case (ll, lr) of {
  (Bin lls _ _ _ _, Bin lrs lrk lrx lrl lrr) | lrs < ratio * lls -> Bin (1 + ls + rs) lk lx ll (Bin (1 + rs + lrs) k x lr r) | otherwise -> Bin (1 + ls + rs) lrk lrx (Bin (1 + lls + size lrl) lk lx ll lrl) (Bin (1 + rs + size lrr) k x lrr r);
  (_, _) -> error "Failure in Data.Map.balanceL"} | otherwise -> Bin (1 + ls + rs) k x l r}};
balanceR :: k -> a -> Map k a -> Map k a -> Map k a;
balanceR k x l r = case l of {
  Tip -> case r of {
  Tip -> Bin 1 k x Tip Tip;
  (Bin _ _ _ Tip Tip) -> Bin 2 k x Tip r;
  (Bin _ rk rx Tip rr@(Bin _ _ _ _ _)) -> Bin 3 rk rx (Bin 1 k x Tip Tip) rr;
  (Bin _ rk rx (Bin _ rlk rlx _ _) Tip) -> Bin 3 rlk rlx (Bin 1 k x Tip Tip) (Bin 1 rk rx Tip Tip);
  (Bin rs rk rx rl@(Bin rls rlk rlx rll rlr) rr@(Bin rrs _ _ _ _)) | rls < ratio * rrs -> Bin (1 + rs) rk rx (Bin (1 + rls) k x Tip rl) rr | otherwise -> Bin (1 + rs) rlk rlx (Bin (1 + size rll) k x Tip rll) (Bin (1 + rrs + size rlr) rk rx rlr rr)};
  (Bin ls _ _ _ _) -> case r of {
  Tip -> Bin (1 + ls) k x l Tip;
  (Bin rs rk rx rl rr) | rs > delta * ls -> case (rl, rr) of {
  (Bin rls rlk rlx rll rlr, Bin rrs _ _ _ _) | rls < ratio * rrs -> Bin (1 + ls + rs) rk rx (Bin (1 + ls + rls) k x l rl) rr | otherwise -> Bin (1 + ls + rs) rlk rlx (Bin (1 + ls + size rll) k x l rll) (Bin (1 + rrs + size rlr) rk rx rlr rr);
  (_, _) -> error "Failure in Data.Map.balanceR"} | otherwise -> Bin (1 + ls + rs) k x l r}};
bin :: k -> a -> Map k a -> Map k a -> Map k a;
bin k x l r = Bin (size l + size r + 1) k x l r
}
