module GHC.Types (List(..), Int, Char, Type) where {
infixr 5 :;
data List a = [] | a : List a;
data Int;
data Char;
data Type
}
