module Main where

main :: IO ()
main =
let value :: Optional Int = Exists 1 >>== \x -> Exists (x * 2) >>= \_a -> Empty :: Optional

instance Semigroup a => Semigroup (Optional a) where
x <> Empty = x
Empty <> y = y
Exists x <> Exists y = Exists (x <> y)

instance Monoid a => Monoid (Optional a) where
mempty = Empty

data Optional a
= Exists a
| Empty

instance Functor Optional where
fmap _ Empty = Empty
fmap f (Exists x) = Exists (f x)

instance Applicative Optional where
pure = Exists
Empty <*> _ = Empty
_ <*> Empty = Empty
Exists f <*> Exists x = Exists (f x)

instance Monad Optional where
Empty >>= _ = Empty
Exists x >>= f = f x
