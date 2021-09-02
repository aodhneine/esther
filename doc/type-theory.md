## Primer to type theory

In this chapter we are going to present an overview of a logic and type theory
that we are going to use for the "primary" implementation of Esther. Instead of
jumping right into HoTT, we are first implementing system based on ETCS[1] /
ZFC[2] / SEAR[3], along with a corresponding logic systems, that allows us to
a) have much cleaner and simpler implementation, and b) be more intuitive and
welcoming to new people.

[1]: https://ncatlab.org/nlab/show/ETCS
[2]: https://ncatlab.org/nlab/show/ZFC
[3]: https://ncatlab.org/nlab/show/SEAR

*Note*: For readers familiar with Homotopy Type Theory, we are going to use a
version of type theory very similar to one used by HoTT.

### Type theory in context of set theory

Thanks to [propositions as types], we need not discern between propositions, a
deductive "part" of the system, and types, the "constructional" part of the
system. Instead, we can identify propositions with types, and regard the
process of proving a theorem as *constructing an object*.

[propositions as types]: https://ncatlab.org/nlab/show/propositions+as+types

A basic expression of type theory is `a : A`, in other words, object `a` is of
type `A`. Although it can seem similar to expression `a ∈ A` in set theory, the
crucial difference that `a : A` is a *judgement*, and not merely a proposition.

A judgement is a statement living in a metalanguage, as opposed to proposition,
which lives in the object language of the deductive system.

This means that, when working in set theory, we can not create propositions of
form "if `a : A` then `b : B`", nor can we prove falseness of `a : A`.

### Definitional equality

We are going to make a distinction between a "definitional" equality, and a
"propositional" one. As a matter of fact, we are not going to introduce the
latter until later on, when we have a solid basis of understanding the
framework we are working in.

A *propositional equality* is a meta-concept, that is, it is not possible to
define it inside the logic framework we are working in. Given two objects `p`
and `q`, we say that they are *propositionally equal* if their definitions are
equal.

For example, given `p` of 2 and `q` of 1 + 1, we can say that `p` and `q` are
propositionally equal. It can be expressed in notation as `p ≡ q`.

*Note*: Readers familiar with type theory will notice that we include a notion
of *computational* equality with definitional one. A reasoning for that can be
found on nLab, under [equality#definitional_equality].

[equality#definitional_equality]: https://ncatlab.org/nlab/show/equality#definitional_equality

When introducing an definition using definitional equality, we are often going
to use symbol `:≡`. Thus, the above definitions can be succinctly written as
`p :≡ 2` and `q :≡ 1 + 1`.

### Types

...

### Functions

Unlike in ZFC, we are going to regard function types as primitive notion. That
means that `f : A → B` means a function, or mapping, from type A to type B.

Function can be defined using λ-abstraction, for example, assuming `f : N → N`,
we can define `f` as:

  f :≡ λ(x:N).x + x

where `N` is the set of natural numbers (we are going to give a precise
definition of `N` later on.)

However, such style of definitions can get jarring quickly, and as such, we
also introduce a second way, by a definition. Assuming the same function, we
can write

  f(x) :≡ x + x

and both of these declarations are then definitionally equal.

*Note*: Readers familiar with functional languages such as Haskell or Idris
will notice that the second style is nothing more than definition by pattern
matching. We will relate to that later.

Another example of a function is a function that, for any type `A` and element
`y : B`, we have a *constant function* `(λ(x:A).y) : A → B`. (We will refine
this example later on when we talk about *dependent functions*.)

---

## Type Theory Primer

A short introduction to type theory as used throught the Homotopy Type Theory,
adapted from Chapter 1 of Homotopy Type Theory [book].

[book]: https://homotopytypetheory.com/book

### Function types

Having types A, B, we can construct the type A → B of functions with the
domain A and the codomain B. We will also refer to them as maps or morphisms
or mappings. Note that, unlike in set theory, functions are a primitive
concept in type theory, instead of being defined as a functional relation
between two sets.

### Types of types, families of types

Given function f : A → B, and an element a : A of the domain, we can apply
the function to get an element of the codomain B, denoted f(a).

A universe is a type, whose element are types. To avoid Russel's paradox, we
introduce a hierarchy of universes, U(0) : U(1) : U(2) : ... Additionally,
we assume that universes are cumulative, i.e. all the elements of U(n) are
also elements of U(n+1) universe.

When some universe U is assumed, we refer to types belonging to U as small
types.

To model a collection of types varying over a given type A, we use functions
B : A → U, whose codomain is a universe. Such functions are called type
families, or sometimes dependent types), and correspond to families of sets
as used in set theory.

An important type family is the *constant* type family, represented as
A → B. It is defined as (λ(a:A).B) : A → B.

### Dependent functions

Dependent functions (Π-types) generalise function types in HoTT. Elements
of a Π-type are functions, whose codomain varies depending on the element of
the domain. They are called Π-types, because they can be viewed as a cartes.
product over a given type.

We denote dependent functions as Π(x:A) B(x) : U.

Note that if B is a contant type family, then we have Π(x:A) (λa:A.B) : U,
which reduces to A → B. That is, it is the ordinary function type.

For example, a function fmax : Π(n:N) Fin(n + 1) returns a largest element
from a non-empty finite set, defined as fmax(n) :≡ n\[n + 1\].

Another example are polymorphic functions, which take a type and then act
on elements of that type. A simplest example is the identity function
id : Π(A:U) A → A, which we can define as λ(A:U).λ(a:A).a (quite mouthful,
isn't it?) We might equivalently define this function as id(A,x) :≡ x.

Another less trivial example is a "swap" function, which changes the order
of arguments of a two-argument function. We can define it as
  swap : Π(A:U) Π(B:U) Π(C:U) (A → B → C) → (B → A → C),
with the defining equation
  swap(A,B,C,f) :≡ λ(b:B).λ(a:A).f(a)(b).

### Product types

Product types are yet another primitive concept in HoTT. Given types A,B : U
we define a type A ⨯ B : U, which we call their cartesian product. Elements
of that type are simply ordered pairs (a,b) : A ⨯ B, where a : A, and b : B.

We can now define projection functions, π : A ⨯ B → A, and σ : A ⨯ B → B,
along with the defining equations π(a,b) :≡ a, and σ(a,b) :≡ b.

However, instead of using a computation rule every time we want to define a
new function, we can use it once, to define a general case, and then use the
resulting function in all other cases. That is, we can define a function
  rec\[A⨯B\] : Π(C:U) (A → B → C) → A x B → C,
with the defining equation
  rec\[A⨯B\](C,g,(a,b)) :≡ g(a)(b).
Then, instead of defining functions such as π and σ directly, we can use our
function as
  π :≡ rec\[A⨯B\](A,λa.λb.a),
  σ :≡ rec\[A⨯B\](B,λa.λb.b).

We refer to such function as the *recursor* for product types. It comes from
the fact that product types are a degenerate case of inductive types, and
for such types the recursor will have to be, necessarily, recursive.

However, if we want to define dependent functions over a product type, we
have to generalise the recursor. Given dependent function C : A ⨯ B → U, we
can define a function f : Π(x:A⨯B) C(x) by taking a function
g : Π(a:A) Π(b:B) C((a,b)) with equation f((a,b)) :≡ g(a)(b).

For example, this way we can prove the propositional uniqueness principle,
which says that every element of A ⨯ B is equal to a pair. Specifically, we
can construct a function
  uniq\[A⨯B\] : Π(x:A⨯B)  ((π(x),σ(x)) = x).
Having this, we can define
  uniq\[A⨯B\]((a,b)) :≡ refl\[A⨯B\].
In case x :≡ (a,b), we can calculate (π((a,b)),σ((a,b))) ≡ (a,b), using the
defining equations for projections. Therefore
  relf\[A⨯B\] : (π((a,b)),σ((a,b))) = (a,b)
is well-typed, since both sides are judgmentally equal.

### Dependent product types

Now, we can generalise a product type to allow the second component of the
pair vary depending on the choice of the first one (similarly as we did when
going from function types to dependent function types.)

This construction is called a dependent pair type, or Σ-type (as in set
theory it corresponds to an indexed sum over a given type.)

Given type A : U and a type family B : A → U, the dependent pair type is
written as Σ(a:A) B(a) : U.

The way to construct is similar to what we've gone through with Π-types. We
have (a,b) : Σ(a:A) B(a) given a : A and b : B(a). If B is a constant type
family, then the Σ-type is simply Σ(a:A) B(a) ≡ A ⨯ B.

We can derive the π-projection from a Σ-type as π : Σ(x:A) B(x) → A by the
defining equation π((a,b)) :≡ a. However, since the type of the second
component of the pair is B(a), the σ-projection needs to be a dependent
function, whose type involves the first projection. This gives us
σ : Π(p:Σ(x:A) B(x)) B(π(p)).

### Coproduct types

Coproducts correspond to disjoin unions in set theory. There are two ways to
construct elements of A + B : U, either as inl(a) : A + B, for a : A, or as
inr(b) : A + B, for b : B (short for left and right injection.)

To construct a non-dependent function f : A + B → C, we need two functions,
each acting on one of the possible alternatives of that type, g0 : A → C,
and g1 : B → C. Then we can define f as
  f(inl(a)) :≡ g0(a),
  f(inr(a)) :≡ g1(a),
that is, by using a case analysis.

We can now derive a recursor,
  rec\[A+B\] : Π(C:U) (A → C) → (B → C) → A + B → C
with the defining equations
  rec\[A+B\](C,g0,g1,inl(a)) :≡ g0(a)
  rec\[A+B\](C,g0,g1,inr(a)) :≡ g1(a).

To construct a dependent function f : Π(x:A+B) C(x) out of a coproduct, we
need to assume a type family C : (A + B) → U, which gives us
  g0 : Π(a:A) C(inl(a)),
  g1 : Π(b:B) C(inr(b)),
which yields us f with the following equations
  f(inl(a)) :≡ g0(a),
  f(inr(a)) :≡ g1(a).

We can now pack that into the inductor for coproducts as
  ind\[A+B\] : Π(C:(A+B)→U) (Π(a:A) C(inl(a))) → (Π(b:B) C(inr(b))) →
    Π(x:A+B) C(x).

### Boolean type

We are now going to introduce a simplest non-trivial type, a type of
booleans 2 : U. It is intended to have exactly two elements, 0, 1 : 2. We
can create it as a coproduct of unit types, 1 + 1.

To derive a function f : 2 → C we need c0,c1 : C
  f(0) :≡ c0,
  f(1) :≡ c1.

The recursor for booleans corresponds to the if-then-else construct
  rec\[2\] : Π(C:U) C → C → 2 → C,
with the defining equations
  rec\[2\](C,c0,c1,0) :≡ c0,
  rec\[2\](C,c0,c1,1) :≡ c1.

Given C : 2 → U, we can derive a dependent function f : Π(x:2) C(x) by
taking c0 : C(0) and c1 : C(1), in which case we have the following equ.
  f(0) :≡ c0,
  f(1) :≡ c1,
which we can package into the inductor as
  ind\[2\] : Π(C:2→U) C(0) → C(1) → Π(x:2) C(x),
with the following equations
  ind\[2\](C,c0,c1,0) :≡ c0,
  ind\[2\](C,c0,c1,1) :≡ c1.

We can now, as an example, construct a proof that every element of 2 is
either 0 or 1. To do this, we need to use the equality type, but we only
need the fact that refl\[x\] : x = x, i.e. that everything is equal to
itself. Thus, we need to construct an element of
  Π(x:2) (x = 0) + (x = 1),
i.e. a function assigning to each x : 2 either an equality x = 0 or x = 1.
We now define this element using the inductor for 2, with
  C(x) :≡ (x = 0) + (x = 1),
and with the two inputs being inl(refl\[0\]) : C(0) and inr(refl\[1\]) : C(1).
In other words, the constructed element of the above function is
  ind\[2\](λx.(x = 0) + (x = 1),inl(refl\[0\]),inr(refl\[1\])).

### Logic, Curry-Howard correspondence

Curry-Howard correspondence allows us to encode propositions as types. That
means that, in addition to constructionist logics, in order to prove that
something is true we merely need to be able to constuct its type. If we are
not able to, then it is proved false.

Propositions are types whose elements are proofs. For example, "true" prop.
is represented as a unit type, since there is only one "proof" of truth.
Contrary, "false" propositions is represented as an empty type, since there
is no "proof" of falsiness.

Proposition "a and b" is represented as a product type A ⨯ B, as it can be
interpreted as a pair of "proofs", (a, b), and we need "proofs" for both of
them.

Proposition "a or b" is a coproduct type A + B, since to "prove" it we need
just either one of them.

Proposition "a implies b" or, equivalently, "if a then b", is a function type
A → B, as if we have a "proof" of a, then we can deduce a truthiness of b.

However, this might need additional explanation. We are going to interpret it
as "if a is inhabited, then we can construct proof of b. if a is not, then we
can not construct proof of b."

Proposition "a iff b" or, equivalently, "a if and only if b" is represented
simply as a product of two implications, (a → b) ⨯ (b → a).

Lastly, we are going to look at a proposition "not a". In other words, we are
to interpret it as "a implies false". This means that it simply is a function
from any type A to an empty type, A → 0.

In other words, that means, if we can construct a proof of A, then we get a
falsiness, and if we can't, then such function type cannot even be created,
thus it is always true.

If we have function f : A → B, and a variable a : A, then by applying f to a
we have, by definition of a function, a value of type B. This is formally
parallel to "modus ponens" in first-order logic, and as such, Curry-Howard
correspondence allows us to relate them together.
