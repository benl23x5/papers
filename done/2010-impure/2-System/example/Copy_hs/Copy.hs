

-- Check that we can have locally quantified vars in class definitions.
--	GHC 6.8.3 allows this with -XRankNTypes

class Copy a where
 copy :: forall b. a -> b


instance Copy Int where
 copy x = error "dude"

instance Copy [a] where
 copy x = error "argh"


class Foo a where
 foo :: forall b. a -> [b] -> [b]


instance Foo Bool where
 foo x y
   = if x 
	then tail y
	else reverse y


instance Foo Char where
 foo x y
  = if x == 'a'
	then tail y
	else [x]


foo2 x y = if x == 'a' then tail y else [x]