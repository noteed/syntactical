-- Some definitions to experiment with GHC associativity and
-- precedence parsing (and thus hopefuly Haskell) rules.

-- X `al7` Y `bl6` Z  =>  B (A X Y) Z
-- X `ar7` Y `br6` Z  =>  B (A X Y) Z
-- X `al5` Y `bl6` Z  =>  A X (B Y Z)
-- X `al5` Y `br6` Z  =>  A X (B Y Z)
-- X `a5`  Y `b6`  Z  =>  A X (B Y Z)
-- X `a5`  Y `br6` Z  =>  A X (B Y Z)
-- X `al5` Y `b6`  Z  =>  A X (B Y Z)
-- Higher precedence binds tighter ((non-)associativity doesn't matter).

-- X `al6` Y `bl6` Z  =>  B (A X Y) Z
-- Same precedence and infix left associates to the left.

-- X `ar6` Y `br6` Z  =>  A X (B Y Z)
-- Same precedence and infix right associates to the right.

-- X `a5`  Y `b5`  Z  =>  parse error
-- X `a6`  Y `bl6` Z  =>  parse error
-- X `al6` Y `b6`  Z  =>  parse error
-- X `ar6` Y `b6`  Z  =>  parse error
-- X `a6`  Y `br6` Z  =>
-- Same precedence and (at least one) non-associative can't be mixed.

-- X `al6` Y `br6` Z  =>  parse error
-- X `ar6` Y `bl6` Z  =>  parse error
-- Same precedence and one left and one right associativity can't be mixed.

data Expr = X | Y | Z
          | A Expr Expr
          | B Expr Expr
  deriving Show

infixl 7 `al7`
al7 = A

infixr 7 `ar7`
ar7 = A

infixl 6 `al6`
al6 = A

infixr 6 `ar6`
ar6 = A

infix 6 `a6`
a6 = A

infixl 6 `bl6`
bl6 = B

infixr 6 `br6`
br6 = B

infix 6 `b6`
b6 = B

infixl 5 `al5`
al5 = A

infix 5 `a5`
a5 = A

infix 5 `b5`
b5 = B

