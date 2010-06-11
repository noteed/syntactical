Distfix expression parsing library
==================================

Syntactical is a Haskell library to parse languages made exclusively, from a
syntactic point of view, of operators and function applications. It is based
on a modified shunting-yard algorithm. The modifications are done to allow
distfix operators and function application written with juxtaposition of
symbols (just like in Haskell).

The resulting parse trees are s-expressions. Further processing to create an
AST specific to the task at hand is left to the user (but this should be
straightforward).

Examples
--------

To create a parser, it is just needed to provide an operator table. To give a
feel of the library at work, here are some examples. The resulting parse trees
(s-expressions) are displayed using the characters ⟨ and ⟩ instead of the
parentheses (this is not enforced by the library).

    -- input => output
    1 + 2 => ⟨+ 1 2⟩
    1 + 2 * 3 => ⟨+ 1 ⟨* 2 3⟩⟩
    f a => ⟨f a⟩
    1 + f a b => ⟨+ 1 ⟨f a b⟩⟩

    -- parentheses for grouping are are just a closed operator
    -- ␣ is the hole, in fact, the + above could be written ␣+␣
    (a) => ⟨(␣) a⟩

    -- if you know that parentheses are only used for grouping
    -- (i.e. it can be viewed as an operator with the semantic
    -- of the identity function), the parser can drop them.
    (a) => a

    -- the parser can also be configured to recognize two symbols
    -- used to write directly s-expressions, here we use again the
    -- characters ⟨ and ⟩. notice that a + b is written in infix
    -- form.
    ⟨* (a + b) ⟨- 1 2⟩⟩ => ⟨* ⟨+ a b⟩ ⟨- 1 2⟩⟩

    -- the library supports mixfix (also called distfix) operators
    true ? 1 : 0 => ⟨␣?␣:␣ true 1 0⟩
    # a => ⟨#␣ a⟩
    a ! => ⟨␣! a⟩
    1 + if true then 1 else a b + c => ⟨+ 1 ⟨if␣then␣else␣ true 1 ⟨+ ⟨a b⟩ c⟩⟩⟩
    [ a | b ] => ⟨[␣|␣] a b⟩

Current state
-------------

The library works but is still a bit rough. Tests don't cover enough cases and
it is simple to overlook one. Documentation is missing (but a look at
tests/Simple is enough to start).