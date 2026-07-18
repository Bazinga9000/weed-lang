#let as-code(x) = if type(x) == str { raw(x) } else { x }

#let op(name, fixity) = [#as-code(name) #text(size: 0.9em, fill: luma(100))[(#as-code(fixity))]]

#let box(c) = {
  block(
    width: 100%,
    fill: luma(248),
    stroke: 1pt + luma(220),
    inset: 10pt,
    radius: 4pt,
    c,
  )
}

#let builtin(name, type_sig, category, aliases: none, fixity: none, examples: none, description) = {
  let formatted-aliases = if aliases == none {
    none
  } else if type(aliases) == array {
    aliases.map(as-code).join(", ")
  } else {
    as-code(aliases)
  }

  show heading.where(level: 2): it => block[
    #set text(size: 11pt)

    *#it.body*
    #if fixity != none [ #text(size: 0.85em, fill: luma(100), weight: "regular")[(#raw(fixity))]]
    ` :: ` #raw(type_sig) #h(1fr) #text(style: "italic", weight: "regular", category)

    #(
      if formatted-aliases != none [
        #set text(size: 0.85em, fill: luma(80))
        #block(width: 100%)[
          *Aliases:* #formatted-aliases
        ]
      ]
    )

    #line(length: 100%, stroke: 0.5pt + luma(150))
  ]

  [#heading(level: 2, numbering: none)[#raw(name)] #label("builtin-" + name)]

  description

  if examples != none {
    v(0.5em)
    box(examples)
  }

  v(1.5em)
}

#let ref-b(name, alias: none) = context {
  // 1. Construct the target label
  let target = label("builtin-" + name)

  // 2. Determine what text to display (falling back to the builtin name)
  let display-text = if alias == none { name } else { alias }
  let display-content = as-code(display-text)

  // 3. Query the document to see if the label exists
  if query(target).len() > 0 {
    // Label exists: link it and color the code text purple
    link(target)[#text(fill: purple)[#display-content]]
  } else {
    // Label missing: apply a red underline (and red text) for the redlink effect
    underline(stroke: red)[#text(fill: red)[#display-content]]
  }
}

#let weed = [#text(green)[WEED]]
#set document(
  title: [#weed --- Specification],
)

#title()
#set heading(numbering: (n, ..rest) => numbering("1.1", calc.max(0, n - 1), ..rest))
#outline()

#let yesmark = [#text(green)[#sym.checkmark]]
#let nomark = [#text(red)[#sym.crossmark]]
= Introduction
#weed (Short for #strong[W]ildly #strong[E]xpressive #strong[E]executor of #strong[D]ice), is an interpreted statically typed pure functional programming language designed for rolling dice (and, more generally, sampling probability distributions).

This is the backing language behind the Super Helpful Dice Simulator Discord bot, implemented in the same repository as #weed.

This specification assumes familiarity with Haskell, as #weed is both implemented in and heavily inspired by it.

Throughout this document, function names in #text(purple)[purple] are builtin or standard library functions and can be clicked to link to their definitions.

= Type System

#weed does not support user-defined data types and has the following list of builtin types:

== Constant Types

The following types have kind `*` and represent values:

- `Number` is the type of numerical data, backed by a numerical tower equipped with roll-metadata. See #link(label("Number Semantics"), "the section on Number Semantics") for more information.
- `Bool` is the type of Booleans, whose members are the literals `True` and `False`.
- `()` is the Unit type, with only one member, the literal `()`.

== Aliases

Some builtins require additional constraints on their inputs, (particularly those that require numeric inputs), and will cause a runtime error if these conditions are not met. The following names will be used to acknowledge these constraints (or as outputs to express that the constraint is guaranteed), but note these are not true types in the language:

- `Real`: A `Number` with imaginary part `0`.
- `NonNegativeReal`: A `Real` greater than or equal to `0`.
- `PositiveReal`: A `Real` strictly greater than `0`.
- `Integer`: A `Real` with no fractional part.
- `Natural`: A non-negative `Integer`.
- `PositiveNatural`: A nonzero `Natural`.
- `NonEmpty a`: A `[a]` that contains at least one element

== Structured Types

The following types have kind `* -> *` and represent structured data:

- `[]` is the type constructor for homogenous lists. `[Number]` is a list of numbers and `[Bool]` is a list of booleans.
- `Dice` is the type constructor for individual dice. Its one argument type is the type generated when rolling the die.
- `Pool` is the type constructor for groups of dice. Internally, `Pool a` tracks two quantities: The actual state of the pool (of type `Dice [a]`) and a canonical generator (`Dice a`). Whenever new rolls are required, such as when using #ref-b("reroll") or #ref-b("explode"), the canonical generator will be used.

== Typeclasses

#weed has the following typeclasses:

=== `Functor`

Types implementing `Functor` can be mapped over. The set of required functions is only #ref-b("map").

`Functor` has the following instances:
#box[```hs
Functor []
Functor Dice
Functor Pool
```]

=== `Monad`

Types implementing `Monad` support sequential effectful computation. This spec is not the place for a detailed intuitive explanation on Monads, so the previous sentence and the statement that the two required functions are #ref-b("return") and #ref-b("bind") will have to suffice.

All instances of `Monad` are required to be instances of `Functor`.

_(Note for Haskellers: `Monad`s also implement #ref-b("ap"), which is technically a member of `Applicative`, but since #weed has no non-Applicative Monads, #ref-b("ap") is implemented in terms of Monad as well. If it becomes necessary to enrich the type system further, this might change.)_

`Monad` has the following instances:
#box[```hs
Monad []
Monad Dice
Monad Pool
```]

=== `Rollable`

Instances of `Rollable` represent dice computations from which a canonical generator can be derived. The only required function on `Rollable` is #ref-b("source").

*Note*: For primitives and simple Pools, the `source` will be a die from which all dice in the Pool are copies of, but after arbitrary transformation this is not always the case. In general, the canonical generator for a Pool represents a "representative distribution" of all dice in the Pool.

Rollable has the following instances:
#box[```hs
Rollable Dice
Rollable Pool
```]

=== `Selector`

Selector is a mulitparameter typeclass that represents predicates on type `a`, be they local (depend on only the target value, like `(`#ref-b("(==)", alias: "==")` 5)`) or global (depend an entire list context, like `(`#ref-b("highest")` 4)`). `Selector a` provides #ref-b("liftMask") to access the most general form of the predicate (of type `[a] -> [Bool]`)

`Selector` has the following implementations:
#box[```hs
Selector a (a -> Bool)
Selector a ([a] -> [Bool])
```]

#label("Number Semantics")
= Number Semantics

#weed numbers consist of two parts: The actual numerical value, alongside metadata that tracks its lineage and which dice-operations affected it.

== The Numeric Tower

Raw numeric values in #weed are represented with a numeric tower, similar to Scheme. There are five internal number types:

- `R`, for exact rational numbers
- `D`, for inexact double-precision floats
- `CR`, for exact complex rationals
- `CD`, for inexact complex double-precision floats
- `N`, the not-a-number value. `N` absorbs all binary operations, with the sole exception that `N ^ 0 = 1`.

Numeric literals are always `R`. For example `0.1` parses as the rational $1/10$.

Whereever possible, operations will produce the most exact value possible, with `D` and `CD` only ever produced when given as an input or when an exact solution cannot be found. For some examples:
#box[```haskell
R (1/10) + R (2/10) = R (3/10)
R (1/10) + D (0.2) = D (0.30000000000000004)
R (-1) ^ R (1/2) = CR (0 + i)
CR (2 - i) + CR (0 + i) = R (2)
CR (1 + i) ^ R (3) = CR (-2 + 2i)
sin (R (0)) = R (0)
```]

Note that for fractional powers of complex numbers, it is only guaranteed that *an* exact root will be found if one exists, `(x ^ y) ^ (1/y)` does not equal `x` in general.

If you explicitly want to convert an exact value into an inexact one, the builtin #ref-b("approximate") can be used.

== Numeric Metadata

Numbers in #weed also carry optional information about their source in the form of metadata. Numbers only carry nontrivial metadata when they were produced by a die or are the result of applying some operation to such a number. Numbers with no metadata are called "pure". In particular, literals are always instantiated pure.

This metadata is expressed with marks and color in frontends and can sometimes carry special semantics.

=== Types of Metadata

There are two fundamnetal types of metadata:

==== Boolean Metadata

Boolean metadata can only take the values `True` and `False`. This type of metadata is generally used for properties intrinsic to the value itself, rather than it sources. To reflect this, accessors for Boolean metadata are prefixed with `is`.

The following Boolean metadata exists:

#table(
  columns: (auto, auto, auto, auto),
  align: center,
  [Metadata], [Accessor], [Relevant Modifiers], [Description],
  [Dropped],
  [#ref-b("isDropped")],
  [
    #ref-b("keep")

    #ref-b("drop")
  ],
  [
    Values dropped from a Pool.

    Binary operations where exactly one value is Dropped will return the other value unchanged.

    Binary operations where both values are Dropped will return a pure `0`.

    Applies no mark.
  ],
)

==== Multiboolean Metadata

Multiboolean metadata tracks how many of a value's ancestors had and did not have a given property. Internally, they are represented by a tuple of two natural numbers. These are usually used for properties such a crits, where it is valuable to know whether _any_ or _all_ of a value's constituent rolls had the property.

As such, each metadata has _two_ accessors, one prefixed with `any` and one prefixed with `all`. The standard library also provides accessors prefixed with `some` to query whether some (but _not_ all) ancestors had the property.

Multiboolean metadata can apply one of two marks depending on whether some or all constituents satisfied it.

#let mbacc(name) = [
  #ref-b("some" + name)

  #ref-b("all" + name)

  #ref-b("any" + name)
]

#table(
  columns: (auto, auto, auto, auto),
  align: center,
  [Metadata], [Accessors], [Relevant Modifiers], [Description],
  [Crit],
  [#mbacc("Crit")],
  [`critsOn`],
  [
    Stores whether or not a die rolled a critical success

    Marked with #sym.star (some) and #sym.star#sym.star (all)
  ],

  [Fail],
  [#mbacc("Fail")],
  [`failsOn`],
  [
    Stores whether or not a die rolled a critical failure

    Marked with #sym.dagger (some) and #sym.dagger.double (all)
  ],

  [Reroll],
  [#mbacc("Reroll")],
  [`reroll`],
  [
    Stores whether or not a die has been rerolled

    Marked with #sym.arrow.ccw (both some and all)
  ],

  [Extra],
  [#mbacc("Extra")],
  [`explode`],
  [
    Stores whether a value was spawned _after_ its source was first rolled (e.g as the result of an explosion hitting)

    Marked with #sym.plus.o (both some and all)
  ],
)

=== Absense of Metadata

For values that carry no metadata (such as literals), all metadata accessor functions will return `False`.

=== Combination of Metadata

Unary and binary operations have the following semantics on metadata:

- Unary operations leave the metadata (or lack therof) of their argument unchanged.
- Binary operations on two pure values are pure.
- Binary operations on one pure value and one value with metadata inherit the metadata of the impure value.
- If `a` and `b` both have metadata, and #raw(sym.diamond) is a binary arithmetic expression (like #ref-b("(+)")), #raw("a " + sym.diamond + " b")...
  - ...is Dropped if *either* `a` or `b` is Dropped
  - ...adds together the true and false counts for Crit, Fail, Reroll, and Extra

=== Frontend Coloring Rules

#let succfail(s) = text(rgb("#cdcd55"))[#as-code(s)]
#let succ(s) = text(rgb("#54fc54"))[#as-code(s)]
#let fail(s) = text(rgb("#fa5454"))[#as-code(s)]
#let extra(s) = text(rgb("#d157d1"))[#as-code(s)]

Frontends will color printed numerical values according to the metadata that they have. The first matching rule in the following table will be used to color a value `x`:

#table(
  columns: (auto, auto, auto),
  align: center,

  [Predicate], [Color], [Example (with Marks)],
  [`someCrit x && someFail x`], [Yellow], [#succfail("1" + sym.star + sym.dagger)],
  [`someCrit x`], [Green], [#succ("20" + sym.star + sym.star)],
  [`someFail x`], [Red], [#fail("1" + sym.dagger.double)],
  [`someExtra x`], [Purple], [#extra("3" + sym.plus.o)],
)

_Note: Precise colors will depend on your frontend's theme, particularly its interpretation of ANSI codes._


= Syntactic Holes

#weed supports the use of the syntactic hole `_` to allow for simpler construction of lambdas. Standard lambdas `(\x -> y)` and `(\x y -> z)` are also supported, this is simply for brevity, and the hole syntax is desugared into regular lambdas.

== Lowering Semantics

A *lifting boundary* is an expression envelope which captures any internal holes and converts the entire envelope into a lambda. The following syntaxes are considered lifting boundaries:

- *Grouping Parenthesis*: A pair of `(` and `)` that enclose an expression which contains either an infix operator or a pipe (#ref-b("(|)"). Parentheses wrapping _only_ standard function application cannot capture holes.
- *Pipe*: A pipe (#ref-b("(|)")). Holes will _never_ lift past pipes.
- *Declaration*: The binding of an expression to a variable.
- *List Literals*: The outer delimiter `[`/`]` or separator `,` of a list literal.
- *Lambda*: A lambda `\foo -> `.

Within a lifting boundary, the expression tree is traversed in pre-order (left to right). Each encountered `_` is replaced by a unique, freshly generated identifier (guaranteed to be distinct from _all_ other identifiers, including other holes).

The entire lifting boundary is then wrapped in a nested series of lambda expressions binding those identifiers in the order in which they were discovered. Consequently, the leftmost hole becomes the outermost argument of the resulting curried lambda.

A bare top-level hole is a syntax error.

== Examples

In these examples, #sym.suit.spade and #sym.suit.heart will represent fresh variable identifiers.

#table(
  columns: (auto, auto, auto),
  align: center,
  [Surface Syntax], [Syntax after Hole Lowering], [Notes],

  [`_ + 2`], [#raw("\\" + sym.suit.spade + " -> " + sym.suit.spade + " + 2")], [Standard unary desugaring.],

  [`_ + _`],
  [#raw("\\" + sym.suit.spade + " " + sym.suit.heart + " -> " + sym.suit.spade + " + " + sym.suit.heart)],
  [Bindings are ordered left to right.],

  [`4d6 | keep (highest _)`],
  [#raw("4d6 | (\\" + sym.suit.spade + " -> keep (highest " + sym.suit.spade + ")")],
  [Pipes block propagation of holes.],

  [`map (_ * 2) [1, 2, 3]`],
  [#raw("map (\\" + sym.suit.spade + " -> " + sym.suit.spade + " + 2) [1, 2, 3]")],
  [The `*` inside parentheses confines the hole.],

  [`map (foo _) [1, 2, 3]`],
  [#raw("\\" + sym.suit.spade + " -> map (foo " + sym.suit.spade + ") [1, 2, 3]")],
  [Parenthesis with only applications do not confine holes.],

  [`let add = _ + _, foo = 5 in add foo 2`],
  [#raw(
    "let add = \\"
      + sym.suit.spade
      + " "
      + sym.suit.heart
      + " -> "
      + sym.suit.spade
      + " + "
      + sym.suit.heart
      + ", foo = 5 in add foo 2",
  )],
  [Let declarations confine holes.],

  [`[someFunc, someOtherFunc, _ + 2]`],
  [#raw("[someFunc, someOtherFunc, \\" + sym.suit.spade + " -> " + sym.suit.spade + " + 2]")],
  [List literals confine holes.],

  [`\x -> _ + x`], [#raw("\\x -> (\\" + sym.suit.spade + " -> " + sym.suit.spade + " + x)")], [Lambdas confine holes.],
)


= Dice Syntax

== Primitive Dice

#weed has support for some common probability distribution and tabletop primitives:
#builtin(
  "d",
  "PositiveNatural -> Dice PositiveNatural",
  "Dice Primitive",
)[
  The classic tabletop die. `d n` samples a positive natural number from ${1, 2, 3, ..., n-1, n}$ uniformly at random. `d%` is also an alias for `d100`.

  This die crits on a roll of $n$ and fails on a roll of $1$.
]

#builtin(
  "s",
  "NonEmpty a -> Dice a",
  "Dice Primitive",
)[
  A set die. `s l` chooses an element from `l` uniformly at random.

  This die crits or fails only if it selects an element that was marked as such.
]

#builtin(
  "f",
  "PositiveNatural -> Dice Integer",
  "Dice Primitive",
)[
  A fudge die. `f n` samples an integer from ${-n, -n+1, ..., -1, 0, 1, ..., n-1, n}$ uniformly at random. `dF` is also a special alias for `f1`.

  This die crits on a roll of $n$ and fails on a roll of $-n$.
]

#builtin(
  "u",
  "PositiveReal -> Dice NonNegativeReal",
  "Dice Primitive",
)[
  A uniform (real) die. `u n` samples a random `D` value uniformly over the interval $[0,n)$.

  This die never crits nor fails.
]

#builtin(
  "gauss",
  "PositiveReal -> Dice Real",
  "Dice Primitive",
)[
  A Gaussian die. `gauss x` samples the Gaussian distribution with parameters $mu = 0, sigma = x$.

  This die never crits nor fails.
]

#builtin(
  "pareto",
  "PositiveReal -> Dice PositiveReal",
  "Dice Primitive",
)[
  A Pareto die. `pareto x` samples the Pareto distribution with shape parameter $alpha = 0$.

  This die never crits nor fails.
]

#builtin(
  "binomial",
  "PositiveNatural -> PositiveReal -> Dice Natural",
  "Dice Primitive",
)[
  A Binomial die. `binomial n p` samples $n$ Bernoulli trials each with success probability $p$ and returns the number of successes.

  This die crits on a roll of $n$ and fails on a roll of $0$.
]

#builtin(
  "coin",
  "Dice Bool",
  "Dice Primitive",
)[
  A coin. Returns `True` or `False` with equal odds. Equivalent to `s[True, False]`.
]

#builtin(
  "circle",
  "PositiveReal -> Dice Number",
  "Dice Primitive",
)[
  A complex circle die. `circle r` selects a complex number uniformly at random with distance `r` from the origin (that is, $x + y i$ where $x^2 + y^2 = r$).

  This die never crits nor fails.
]

== Special Dice Syntax

#weed has special syntax to allow for easy specification of primitive dice.

If a primitive die that takes only one parameter is immediately suffixed with a literal of its required type, instead of being treated as a single identifier, it will be treated as function application. For example, `d6` is syntactic sugar for `d 6`, and `s[1,1,2,3]` is sugar for `s [1,1,2,3]`.

Similarly, if a primitive die is immediately prefixed with a literal number, this is desugared into an application of the die replication operator #ref-b("(#)"). For example, `8d6` is syntactic sugar for `8 # d 6`.


= Type Coersion Rules

To enable commonly used mixed-type operations (like `3d6 + 5`) to work without manual type coersion, #weed employs a strategy of coersion for applications `f x`. The first row in the following table that matches the types of `f` and `x` will be applied.

#table(
  columns: (auto, auto, auto, auto, auto),
  align: center,
  [Rule Name], [Type of `f`], [Type of `x`], [Transformed `f x`], [Coerced Type],

  [Bare Application], [`a -> b`], [`a`], [`f x`], [`b`],

  [Pool Mapping], [`[a] -> b`], [`Pool a`], [#ref-b("mapP")` f b`], [`Dice b`],

  [Pool Collapse (Direct)], [`Dice Number -> b`], [`Pool Number`], [`f (`#ref-b("collapse")` x)`], [`b`],

  [Pool Collapse (Indirect)],
  [`Number -> b`],
  [`Pool Number`],
  [#ref-b("map")` f (`#ref-b("collapse")` x)`],
  [`Dice b`],

  [Pool Collapse (Applicative)],
  [`Dice (Number -> b)`],
  [`Pool Number`],
  [#ref-b("ap")` f (`#ref-b("collapse")` x`)],
  [`Dice b`],

  [Implicit #ref-b("map")], [`a -> b`], [`Functor f => f a`], [#ref-b("map")` f x`], [`f b`],

  [Implicit #ref-b("ap")], [`Monad m => m (a -> b)`], [`m a`], [#ref-b("ap")` f x`], [`m b`],

  [Scalar Promotion (Direct)], [`Monad m => m a -> b`], [`a`], [`f (`#ref-b("return")` x)`], [`m b`],

  [Scalar Promotion (Indirect)], [`Monad m => m (a -> b)`], [`a`], [#ref-b("ap")` f (`#ref-b("return")` x)`], [`m b`],
)

#counter(heading).update(0)

#set heading(numbering: (..nums) => {
  let vals = nums.pos()
  if vals.len() == 1 {
    "Appendix " + numbering("A:", vals.first())
  } else {
    numbering("A.1", ..vals)
  }
})
= Builtin Functions

#builtin(
  "(+)",
  "Number -> Number -> Number",
  "Arithmetic",
  fixity: "infixl 6",
  examples: [
    ```haskell
    0.1 + 0.2   -- 3/10
    5 + 5       -- 10
    10.2 + 1    -- 11.2
    ```
  ],
)[
  Adds two numbers together.
]

#builtin(
  "(#)",
  "PositiveNatural -> Dice a -> Pool a",
  "Dice Operations",
  fixity: "infixr 5",
)[
  Repeatedly rolls a die a number of times and assembles them into a `Pool`.
]

#builtin(
  "map",
  "Functor f => (a -> b) -> f a -> f b",
  "Structures",
  aliases: [#op(`<$>`, `infixl 4`)],
  examples: [
    ```haskell
    (*2) <$> [1, 2, 3, 4, 5] -- [2, 4, 6, 8, 10]
    (*3) <$> d6 -- s[3, 6, 9, 12, 15, 18]
    ```
  ],
)[
  Map a function over a `Functor`.
]

#builtin(
  "mapP",
  "([a] -> b) -> Pool a -> Dice b",
  "Structures",
  examples: [
    ```hs
    mapP sum (20d6) -- A die rolling the single sum value
    -- (mapP sum) is implicitly called quite often when working with Pools of Numbers
    ```
  ],
)[
  Apply a function to the results of a Pool in aggregate, collapsing it into a Dice.
]

#builtin(
  "return",
  "Monad m => a -> m a",
  "Structures",
  examples: [
    As `[Number]`:
    ```haskell
    return 1 -- [1]
    ```
    As `Dice Number`:
    ```haskell
    return 3 -- s[3]
    ```
    As `Pool Number`:
    ```haskell
    return 7 -- 1s[7]
    ```
  ],
)[
  Lift a value into a monad.
]

#builtin(
  "bind",
  "Monad m => m a -> (a -> m b) -> m b",
  "Structures",
  examples: [
    ```haskell
    [1, 2, 3] >>= (\x -> [x^2, x^3, x^4]) -- [1, 1, 1, 4, 8, 16, 9, 27, 81]
    d6 >>= (\m -> if m == 6 then d1000 else m) -- s[1, 2, 3, 4, 5, d1000]
    ```
  ],
  aliases: [
    #op(`>>=`, `infixl 1`)
  ],
)[
  Sequentially compose two monadic actions, passing values produced by the first as arguments to the second.
]

#builtin(
  "liftMask",
  "Selector s a => s -> [a] -> [Bool]",
  "Filtering",
  examples: [
    ```hs
    liftMask (_ == 1) [1, 2, 3] -- [True, False, False]
    ```
  ]
)[
  Lifts a `Selector` into the corresponding global predicate `[a] -> Bool`.

  If `s = [a] -> [Bool]`, `liftMask` is the identity. If `s = a -> Bool`, `liftMask` is #ref-b("map").
]

#builtin(
  "source",
  "Rollable r => r a -> Dice a",
  "Dice",
  examples: [
    ```haskell
    source d6 -- d6
    source 8d7 -- d7
    source (if 4coin then d6 else d3) -- if coin then d6 else d3
    ```
  ],
)[
  Extracts the canonical generator from a `Rollable`.
]

#builtin(
  "reroll",
  "Rollable r, Selector a s => s -> r a -> r a",
  "Dice Modifiers",
  examples: [
    ```hs
    4d6 | reroll (== 1) -- will only ever produce 2-6
    ```

  ],
)[
  Rerolls a die (or all dice in a Pool) that satisfy the `Selector` repeatedly until they do not.

  Adds Reroll metadata (whether the die was rerolled or not) to all affected output values.
]

#builtin(
  "explode",
  "Rollable r, Selector a s => s -> r a -> Pool a",
  "Dice Modifiers",
)[
  Each die that satisfies the Selector will spawn an additional copy of itself (whose output is tagged Extra) and add it to the resulting Pool. This applies recursively.
]

#builtin(
  "sum",
  "[Number] -> Number",
  "Arithmetic",
  examples: [
    ```haskell
    [1, 2, 3, 4, 5] | sum -- 15
    [1, 2, 3, 4, 5] | sum -- 12
    ```
  ],
)[
  Compute the sum of a list of Numbers. Ignores values marked as `dropped` by default.
]

#builtin(
  "approximate",
  "Number -> Number",
  "Arithmetic",
  examples: [
    ```hs
    approximate (3/10) = 0.30000000000000004
    ```
  ],
)[
  Force an exact `R` or `CR` into an inexact `D` or `CD`.
]

#builtin(
  "highest",
  "Natural -> [Real] -> [Bool]",
  "List Operations",
  examples: [
    ```hs
    highest 3 [1, 2, 3, 4, 5] -- [False, False, True, True, True]
    highest 3 [5, 2, 5, 5, 5] -- [True, False, True, True, False]
    ```
  ]
)[
  Return a bit mask with the highest $n$ elements set. Ties are broken earliest to latest in the list.
]

#builtin(
  "lowest",
  "Natural -> [Real] -> [Bool]",
  "List Operations",
  examples: [
    ```hs
    lowest 3 [8, 1, 2, 5, 3] -- [False, True, True, False, True]
    lowest 3 [1, 8, 1, 1, 1] -- [True, False, True, True, False]
    ```
  ]
)[
  As #ref-b("highest"), but the bit mask has the lowest $n$ elements set.
]

// == Standard Library Functions
