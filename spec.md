# WEED - Specification

Weed (Short for Wildly Expressive Executor of Dice) is an interpreted "not entirely pure" functional programming language optimized for rolling dice and evaluating probability distributions.

## Typing

Weed is statically typed with strict type inference. 
- **Number**: Represents both integers and floats.
- **Bool**: Has two members, `True` and `False`.
- **Unit `()`**: The empty tuple type.
- **Lists `[a]`**: Homogeneous collections of type `a`.
- **Functions `(a -> b)`**: First-class functions.
- **Dice Monad `Dice a`**: Represents a random computation that, when sampled by the runtime framework, yields a value of type `a`.
- **Dice Pool Monad `Pool a`**: As `Dice a`, but represents a collection of dice whose original generator can be tracked (for instance `d6 :: Dice Number`, but `4d6 :: Pool Number`) 

The following typeclasses exist:
- **Functor `Functor d`**: Implemented by `[]`, `Dice`, and `Pool`, provides `fmap` for mapping over values of type `a` in `d a`.
- **Monad `Monad d`**: Implemented by `[]`, `Dice`, and `Pool`, provides `return` and `>>=` for sequencing computations.
- **Rollable `Rollable d`**: Implemented by `Dice` and `Pool`, provides `source :: Rollable d => d a -> Dice a` for extracting the original generating dice which extracts the single die which all rolls represented by the value share. For `Dice a`, `source` is the identity. For `Pool a`, `source` tracks the original generating die (for instance, `source 4d6 = d6`.)

## Dice Syntax

### Primitive Dice
Dice can be declared using standard tabletop notation or explicit distribution functions:

| Syntax | Name | Type | Distribution / Behavior |
| :--- | :--- | :--- | :--- |
| `dX` | Standard Dice | `Dice Number` | Uniformly random integer from `1..X` (e.g., `d6`). `d%` is an alias for `d100`. |
| `sL` | List Dice | `Dice a` | Uniformly random element picked from list `L :: [a]`. If the list is empty, a runtime error is thrown. |
| `fX` | Fudge Dice | `Dice Number` | Uniformly random integer from `-X..X`. `dF` is an alias for `f1`. |
| `uX` | Uniform Dice | `Dice Number` | Uniformly random float from `0..X`. |
| `gauss X` | Gaussian Dice | `Dice Number` | Gaussian distribution with mean 0 and standard deviation `X`. |
| `pareto X` | Pareto Dice | `Dice Number` | Pareto distribution with shape parameter `X`. |
| `binomial X p` | Binomial Dice | `Dice Number` | Binomial distribution with `X` trials and probability `p`. |
| `coin` | Coin Dice | `Dice Bool` | Uniformly random `True` or `False`. |
| `circle X` | Circle Dice | `Dice Number` | Uniformly random complex number with magnitude `X`. |

### Primitive Dice Pools

Primitive dice with exactly one parameter can be prefixed with a numeric literal to create a dice pool of that size. For example, `2d6 :: Pool Number` and desugars to `2 # d6` (see Core Utilities)

---
### Application and Piping
- **Juxtaposition:** Whitespace juxtaposition strictly represents standard left-associative function application: `a b :: t2` (given `a :: t1 -> t2` and `b :: t1`).
- **Piping (`|`):** The pipe operator strictly represents reverse application: `a | b` desugars at parse-time into `b a`.

---

### Dice Modifiers & Helpers

Modifiers are functions that alter or filter dice computations. 

#### Core Utilities
* `(#) :: Number -> Dice a -> Pool a`  
  Evaluates a dice computation `n` times and collects the results.
* `draw :: Number -> Dice a -> Pool a`  
  Evaluates a dice computation `n` times using rejection sampling to guarantee the resulting list contains entirely unique values. Will return undersized lists if `n` is larger than the number of unique values available.

#### Rerolling Modifiers

* `reroll :: Rollable d => (a -> Bool) -> d a -> d a`: Rerolls each individual computation until the result satisfies the predicate.
* `rerollOnce`: As `reroll`, but only performs one iteration.

#### Collection Modifiers
Because modifiers like `keep` and `drop` manipulate arrays inside the Monad, they target `Dice [a]` structures:

* `keep :: ([a] -> [a]) -> Pool a -> Pool a`  
  Applies a list-filtering function to the rolled results.
* `drop :: ([a] -> [a]) -> Pool a -> Pool a`  
  The inverse of keep; removes elements matching the filter configuration.

#### Filter Predicates (`[a] -> [a]`)
* `highest :: Number -> [a] -> [a]` (Retains the top `n` values).
* `lowest :: Number -> [a] -> [a]` (Retains the bottom `n` values).
* `unique :: [a] -> [a]` (Deduplicates the list).

*Example Usage:*  
`4d6 | keep (highest 3)` rolls four d6s and keeps the top three.

#### Meta-Modifiers
* `threshold :: Rollable r => (Number -> Bool) -> (r a -> r a) -> r a -> r a`  
  Alters the trigger condition of another modifier (e.g., modifying when a die explodes).

---

## Number Metadata

Numbers in WEED are by default complex doubles. To construct complex literals, use `:+` and `:-`. (`3 + 4i` is `3 :+ 4`.)

Numerical primitives inside the execution layer carry tag metadata tracking their lineage (e.g., fields marking a number as `dropped` or was a critical success or failure). Built-in functions like `sum` natively scan this metadata to automatically omit numbers flagged as dropped from final calculations.

the functions `critsOn :: Rollable r => (a -> Bool) -> r a -> r a` and `failsOn :: Rollable r => (a -> Bool) -> r a -> r a` apply a predicate to the dice result, marking it as a critical success or failure if it satisfies the predicate (primitive dice have reasonable defaults for critical success/failure detection). 

The functions `dropped?`, `crit?` and `fail?`, all `Number -> Bool` functions, return `True` if the number is marked as dropped, critical success or failure respectively.

Numbers *not* produced by dice are tagged with `pure` and can be queried with `pure?`. Pure numbers have no other metadata.

The metadata inheritance rules for operations are as follows.
- Unary operations preserve the metadata of their operand.
- For binary operations:
  - If one of the operands is `pure?`, the result inherits the metadata of the other operand.
  - If both operands are `pure?`, the result is `pure?` and has no other metadata.
  - If neither operand is `pure?`, the result is `dropped?`/`crit?`/`fail?` if and only if both operands are.
    * It is legal for a value to be both `crit?` and `fail?` simultaneously. In printed output, such a value is marked as both.

---

## Holes

WEED supports the use of the underscore `_` as a syntactic hole to automatically generate lambda expressions. Holes are conditionally lifted to the nearest enclosing binary operation block, generating multi-parameter functions ordered from left to right. Standard lambdas `(\x -> y)` and `(\x y -> z)` are also supported; holes are provided merely for brevity.

### Lowering Semantics

When the compiler lowers the surface AST into the core AST, expressions containing holes are desugared using the following rules:

1. **Lifting Boundary**: A lifting boundary is an expression envelope which captures any internal holes and converts the entire envelope into a lambda. A hole-lifting boundary is triggered by the nearest enclosing:
   - **Grouping Parenthesis:** `(..)` that enclose an expression containing an infix operator or pipe. Parenthesis wrapping only standard function application cannot capture holes.
   - **Pipe Limb:** The expressions on either side of a pipe.
   - **Let Body:** The expression after `=` in a `let` binding.
   - **List Literal:** The expressions inside a list literal `[..]`.
   - **Lambda Body:** The body of a lambda `\x -> ...`
2. **Left-to-Right Binding**: Within a lifting boundary, the expression tree is traversed in pre-order (left-to-right). Each encountered `_` is replaced by a unique, freshly generated variable name (guaranteed to differ from all others).
3. **Lambda Wrap**: The entire binary operation subtree is then wrapped in a nested series of lambda expressions binding those variables in the order they were discovered. Consequently, the leftmost hole becomes the outermost argument of the resulting function.
4. **Boundary Isolation via Pipes**: Pipes act as structural barriers. Holes never lift beyond a pipe.
5. **Top-Level Isolation**: A bare hole `_` sitting at the top level of a script, outside of any binary operation context, is illegal and results in a compilation error.

### Examples

| Surface Syntax | Desugared Core Syntax | Notes |
| :--- | :--- | :--- |
| `_ + 2` | `\u1 -> u1 + 2` | Standard unary-style mapping. |
| `_ * _` | `\u1 -> \u2 -> u1 * u2` | Ordered left-to-right. |
| `4d6 \| keep (highest _)` | `4d6 \| (\_u1 -> keep (highest _u1))` | Pipe barrier: The hole is isolated to the right limb. |
| `map (_ * 2) [1, 2, 3]` | `map (\_u1 -> _u1 * 2) [1, 2, 3]` | Parenthetical Isolation: Parentheses confine the hole, passing a pure function directly into map. |
| `let add = _ + _ in add 5 2` | `let add = \_u1 -> \_u2 -> _u1 + _u2 in add 5 2` | Let Binding: Captured instantly by the value definition. |

--- 

## Type Coersion

If a function application (or an `if` block, which desugars to application) `f x` fails to type-check, WEED will attempt to perform the following coersions, in order:

1. Pool Mapping
  - If `f :: [a] -> b` and `x :: Pool a`, the type-checker implicitly maps the list function over the pool's generated values, returning `Dice b`.
2. Pool Collapse
  - If `f :: Dice Number -> b` or `f :: Number -> b`, and `x :: Pool Number`, the `Pool` is collapsed to a `Dice Number` using `collapse :: Pool Number -> Dice Number`, which produces a `Dice Number` which "forgets" the source information and is treated as a single die, summing the pool.
3. Implicit Applicative
  - (a - Implicit `fmap`) If `f :: a -> b` and `x :: Rollable r => r a`, implicitly `fmap` (`fmap :: Functor f => (a -> b) -> f a -> f b`) `f` over `x` to produce `Rollable b`.
  - (b - Implicit `<*>`) If, for some `Rollable r`, `f :: r (a -> b)` and `x :: r a`, implicitly `<*>` (`<*> :: Applicative f => f (a -> b) -> f a -> f b`) `f` over `x` to produce `r b`.
4. Scalar Promotion
  - If, for some `Rollable r`, `f :: r a -> b` and `x :: a`, implicitly lift `x` into a deterministic `r a` using `return`, then *retry step 3b*.
