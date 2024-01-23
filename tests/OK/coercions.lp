constant symbol Nat: TYPE;
constant symbol Float: TYPE;
constant symbol round (_: Float): Nat;
coerce_rule coerce Float Nat $t ↪ round $t;
constant symbol succ (_: Nat): Nat;
assert (x: Float) ⊢ succ x ≡ succ (round x);

// Coercion with holes
constant symbol List: TYPE;
constant symbol SortedList: TYPE;
constant symbol sortedp (_: List): TYPE;
constant symbol sorted (l: List) (_: sortedp l): SortedList;
coerce_rule coerce List SortedList $l ↪ sorted $l _;  
constant symbol min (_: SortedList): Nat;

assert (l: List) ⊢ min l: Nat;
assert (l: List) ⊢ min (sorted l _): Nat;

// Parametric coercions
constant symbol Set: TYPE;
injective symbol El (_: Set): TYPE;
constant symbol nat: Set;
constant symbol float: Set;
symbol round! (_: El float): El nat;
coerce_rule coerce (El float) (El nat) $t ↪ round! $t;
constant symbol cell (_ _: Set): Set;
constant symbol cons [a: Set] [b: Set] (x: El a) (y: El b): El (cell a b);
symbol car [a: Set] [b: Set] (_: El (cell a b)): El a;
symbol cdr [a: Set] [b: Set] (_: El (cell a b)): El b;
coerce_rule coerce (El (cell $a $b)) (El (cell $c $d)) $t ↪
  cons (coerce (El $a) (El $c) (car $t))
       (coerce (El $b) (El $d) (cdr $t));
assert (x y: El float) ⊢ cons x y: El (cell nat nat);
