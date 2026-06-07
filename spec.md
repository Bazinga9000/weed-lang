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
- **Dice Pool Monad `Pool a`**: As `Dice a`, but represents a collection of dice equipped with a canonical generator for producing new values (for instance `d6 :: Dice Number`, but `4d6 :: Pool Number`). For primitive pools, the this generator is simply the source die which was repeatedly rolled to produce the pool, though after arbitrary transformations is is neither guaranteed that this generator is primitive, nor is it guaranteed that every die in the pool is the same as this generator. Internally, `Pool a` is stored as `(Dice [a], Dice a)`, with the first half being the actual pool and the right half being the generator.

The following typeclasses exist:
- **Functor `Functor d`**: Implemented by `[]`, `Dice`, and `Pool`, provides `fmap` for mapping over values of type `a` in `d a`.
- **Monad `Monad d`**: Implemented by `[]`, `Dice`, and `Pool`, provides `return` and `>>=` for sequencing computations.
- **Rollable `Rollable d`**: Implemented by `Dice` and `Pool`, provides `source :: Rollable d => d a -> Dice a` for acquiring the canonical generator used to produce new values. For `Dice a`, `source` is the identity. For `Pool a`, `source` produces the tracked canonical generator. 

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

Numbers *not* produced by dice are tagged with `pure` and can be queried with `isPure :: Number -> Bool`. Pure numbers have no other metadata.

The metadata inheritance rules for operations are as follows.
- Unary operations preserve the metadata of their operand.
- For binary operations:
  - If one of the operands `isPure`, the result inherits the metadata of the other operand.
  - If both operands are `isPure`, the result `isPure` and has no other metadata.
  - Otherwise, see the following table:
  
| Metadata         | Accessor Function (type `Number :: Bool`) | Inheritance Rule (`a ? b` has the tag if...) | Modifier                                                              | Special Properties                                                  | Default                                     |
|------------------|-------------------------------------------|----------------------------------------------|-----------------------------------------------------------------------|---------------------------------------------------------------------|---------------------------------------------|
| Dropped Value    | `isDropped`                               | `isDropped a` OR `isDropped b`               | Use `keep/drop` to keep or drop dice from Pools based on a predicate. | Can be cleared with `unDrop :: Number -> Number`. Ignored by `sum`. In repls, colored gray. | not `isDropped`                             |
| Critical         | `isCrit`                                  | `isCrit a` OR `isCrit b`                     | `critsOn :: Rollable r => (Number -> Bool) -> r a -> r a`             | Displayed in REPLs with ★ and colored green.                        | Die dependent maximal value (if one exists) |
| Perfect Critical | `isPerfectCrit`                           | `isPerfectCrit a` AND `isPrefectCrit b`      | Not modifiable separately, use `critsOn`.                             | Displayed in REPLs with ★★ and colored green.                       | N/A                                         |
| Failure          | `isFail`                                  | `isFail a` OR `isFail b`                     | `failsOn :: Rollabe r => (Number -> Bool) -> r a -> r a`              | Displayed in REPLs with † and colored red.                          | Die dependent minimal value (if one exists) |
| Total Failure    | `isTotalFail`                             | `isTotalFail a` AND `isTotalFail b`          | Not modifiable separately, use `failsOn`.                             | Displayed in REPLs with ‡ and colored red.                          | N/A                                         |
| Extra            | `isExtraDie`                              | `isExtraDie a`  OR `isExtraDie b`            | N/A                                                                   | Displayed in REPLs with ! and colored purple.                       | Only producible with `explode`.             |

*Note on crits and failures:* `isPerfectCrit` always implies `isCrit` and `isTotalFail` implies `isFail`.

*Note on colors:* A value can only take one color at a time, so the following priority is applied:

1. A value both `isCrit` and `isFail` (possible either manually, or with `d1`) will be colored yellow.
2. A value that is exactly one of `isCrit` or `isFail` gets that color.
3. `isExtraDie` values get their purple colors.

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
| `map (foo _) [1, 2, 3]` | `\_u1 -> map (foo u1) [1, 2, 3]` | Parenthetical Non-Isolation: Parenthesis with no infix or pipe do *not* confine the hole. |
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
