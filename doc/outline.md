Compiler needs to parse source code into an abstract syntax tree (AST). Then we
expand syntax definitions and perform type checking and proof verification in
the kernel.

Next, we build a single static assignment form (SSA), which allows us to easily
do optimisations (loop unrolling, constant propagation), and check control flow
graph (CFG).

Finally, we can run the resulting SSA by converting it into machine code, which
is then executed on the user's computer. We can also convert it into a bytecode
that will be run on Purr virtual machine, or we can evaluate the SSA directly,
which is VERY slow.

---

Here we are going to present an outline of type theory as used in the Esther
proof assistant.

### Types

We introduce a primitive notion of a type, a collection of elements. A notion
`a : A` represents an element `a` of type `A`.

We also introduce a notion of *judgemental equality*, that is, having defined
elements `a b` of type `A`, we say that they are *definitionally* equal if
their definitions are the same. We then can write that `a ≡ b`.

### Function types

Another notion we introduce is of the function type. Unlike in set theory, we
define it to be a primitive notion, and not a ordered pair of elements that
belong to a relation. We can represent a function `f` from type `A` to type `B`
as `f : A → B`.

We can construct elements of function types by using λ-abstraction. Given, say,
`f : ℕ → ℕ`, we can define it for example as `(λ(x:ℕ).x + x) : ℕ → ℕ`. We can
then associate `f` with the above definitions using definitional equality that
`f :≡ λ(x:ℕ).x + x`.

However, using λ-abstraction directly can get tedious pretty quickly. We can
then use other way of defining functions, by a *direct definition*. This means,
given the above function, we can provide a definition `f(x) :≡ x + x`, which is
definitionally equivalent.

### Propositions as types

Curry-Howard correspondence allows us to relate propositions and types. More
precisely, it allows us to say that, in order to prove a proposition of type
`P`, we need to provide a way to construct an element `p : P`. Such element
is then called a *proof* or *witness* of the proposition, and such proposition
is said to be *inhabited*.

In a case that it is impossible to provide a way to create an element of a
proposition, such proposition is said to be *proven false*, or that type `P`
is *uninhabited*.

A canonical examples are unit and empty types, which stand respectively for
types of true and false propositions. Note, that even though there are many
possible choices for the truth and falseness, we choose unit and empty types
as a canonical way of representing them. This allows us to avoid having to
define our own definition of them in every proof we perform.

However, not all types form proper propositions. Take as an example a type of
function from `ℕ` to `ℕ`, i.e. `f : ℕ → ℕ`. It would be nonsensical to consider
it a proposition, and for this reason we say introduct a notion of *mere
propositions*. Mere propositions are propositions that are only *some types*,
which we can express in a relation `prop ⊂ type`, i.e. propositions are a
subset of all types.

### Identity types

An important aspect of type theory is the existence of *identity types*. For
any two types `α β : U` there is a type `α = β` iff  types `α` and `β` are
propositionally equal.

For any type `α : U`, there exists type `α = α`, i.e. every type is equal to
themselves. This is also called a *reflexivity* type, and is often written as
`refl[α] : α = α`.

We can also write `refl[α,β] : α = β` to denote types that are propositionally
equal. In such case, the existence of an element of `refl` is a proof of these
types being equal.

### Natural numbers

We can formulate a type of natural numbers through a means of *induction*. If
there is an element `0 : nat`, and if, for all `n : nat`, `s(n) : nat`, then
type `nat` is equivalent to the type of natural numbers.

We then call `0` and `s(n)`, for `n : nat`, constructors of the type `nat`.
