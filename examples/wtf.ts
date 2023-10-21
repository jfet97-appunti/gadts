export {}

// data Wtf a where
//   Nul :: Wtf a -- isomorphic to: forall a'. Wtf a'
//   Wnt :: Int -> Wtf Int
//   SimpleEx :: forall a. a -> Wtf a
//   ComplexEx :: forall a b c. a -> b -> c -> Wtf (a, b)

type Wtf<A> =
  | Nul<A>
  | Wnt<A>
  | SimpleEx<A>
  | ComplexEx<A>


// --------------------------- Nul<A> ---------------------------
// (Functor Wtf) => Wtf a' ~ (forall r. (a' -> r) -> Wtf r)

interface NulU<A, _A> {
    eq: (_: _A) => A
}

interface Nul<A> {
    nul: <R>(_: <_A>(_: NulU<A, _A>) => R) => R
    _tag: "Nul"
}

// this constructor creates an encoded value of type forall _A. Wtf<_A>
// note: _A sort of loses its existential nature because it got equated to the universal type declared by Wtf
function Nul_y<_A>() {
    return <R>(eq:(_: _A) => R): Wtf<R> => ({
        nul: cont => cont({
          eq,
        }),
        _tag: "Nul",
    } satisfies Nul<R>)
}

// --------------------------- Wnt<A> ---------------------------
// (Functor Wtf) => Wtf number ~ (forall r. (number -> r) -> Wtf r)

interface Wnt<A> {
    value: number
    eq: (_: number) => A
    _tag: "Wnt"
}

// this constructor lifts a number into an encoded Wtf<number>
function Wnt_y(n: number) {
    return <R>(eq:(_: number) => R): Wtf<R> => ({
        value: n,
        eq,
        _tag: "Wnt",
    } satisfies Wnt<R>)
}

// ------------------------ SimpleEx<A> ------------------------
// (Functor Wtf) => Wtf a ~ (forall r. (a -> r) -> Wtf r)

interface SimpleExU<A, _A> {
    _a: _A,
    eq: (_: _A) => A
}

interface SimpleEx<A> {
    simple: <R>(_: <_A>(_: SimpleExU<A, _A>) => R) => R
    _tag: "SimpleEx"
}

// given a value of type _A this constructor creates an encoded value of type Wtf<_A>
// note: _A sort of loses its existential nature because it got equated to the universal type declared by Wtf
function SimpleEx_y<_A>(_a: _A) {
    return <R>(eq:(_: _A) => R): Wtf<R> => ({
        simple: cont => cont({
          _a,
          eq,
        }),
        _tag: "SimpleEx",
    } satisfies SimpleEx<R>)
}

// ---------------------- ComplexEx<A> ----------------------
// (Functor Wtf) => Wtf (a, b) ~ (forall r. ((a, b) -> r) -> Wtf r)

interface ComplexExU<A, _A, _B, _C> {
    _a: _A,
    _b: _B,
    _c: _C,
    eq: (_: [_A, _B]) => A
}

interface ComplexEx<A> {
    complex: <R>(_: <_A, _B, _C>(_: ComplexExU<A, _A, _B, _C>) => R) => R
    _tag: "ComplexEx"
}

// given three values of type _A, _B, _C this constructor creates an encoded value of type Wtf<[_A, _B]>
// note: _C maintains its existential nature, while _A and _B sort of lose their existentiality
// because they got equated to the universal type declared by Wtf
function ComplexEx_y<_A, _B, _C>(_a: _A, _b: _B, _c: _C) {
    return <R>(eq:(_: [_A, _B]) => R): Wtf<R> => ({
        complex: cont => cont({
          _a,
          _b,
          _c,
          eq,
        }),
        _tag: "ComplexEx",
    } satisfies ComplexEx<R>)
}

// ---------------- Functor instance for Wtf<A> ----------------

// Functor instance for Wtf<A>, mandatory for Yoneda encoding
function fmap<A, B>(f: (_: A) => B, w: Wtf<A>): Wtf<B> {
    switch(w._tag) {
        case "Nul": {
          return w.nul(_ => Nul_y<B>()(_ => _))
        }
        case "Wnt": {
          return Wnt_y(w.value)(_ => f(w.eq(_)))
        }
        case "SimpleEx": {
          return w.simple(w => SimpleEx_y(f(w.eq(w._a)))(_ => _))
        }
        case "ComplexEx": {
          return w.complex(w => ComplexEx_y(w._a, w._b, w._c)(_ => f(w.eq(_))))
        }
    }
}

// ---------------- Yoneda encoding & decoding ----------------

// (Functor f) => f a -> (forall r. (a -> r) -> f r)
// where f is fixed to Wtf for simplicity
function to_yoneda<A>(decoded: Wtf<A>): <R>(eq: (_: A) => R) => Wtf<R> {
    return eq => fmap(eq, decoded)
}

// (Functor f) => (forall r. (a -> r) -> f r) -> f a
// where f is fixed to Wtf for simplicity
function from_yoneda<A>(encoded: <R>(eq: (_: A) => R) => Wtf<R>): Wtf<A> {
    return encoded(_ => _)
}

// examples

const ex = from_yoneda(to_yoneda(wnt(1)))
//    ^?

// ------------------- helpers -------------------

function nul<A>(): Wtf<A> {
    return from_yoneda(Nul_y<A>())
}

function wnt(n: number): Wtf<number> {
    return from_yoneda(Wnt_y(n))
}

function simple<A>(_a: A): Wtf<A> {
    return from_yoneda(SimpleEx_y(_a))
}

function complex<A, B, C>(a: A, b: B, c: C): Wtf<[A, B]> {
    return from_yoneda(ComplexEx_y(a, b, c))
}

// ------------------- Eval -------------------

function evalWtf<A>(w: Wtf<A>): A {
  switch(w._tag) {
    case "Nul": {
      throw new Error("Nul")
    }
    case "Wnt": {
      return w.eq(w.value)
    }
    case "SimpleEx": {
      return w.simple(w => w.eq(w._a))
    }
    case "ComplexEx": {
      return w.complex(w => w.eq([w._a, w._b]))
    }
  }
}

// examples

const complexWtf = fmap(([s, n]) => `${s}${n}`, complex("a", 1, true))

console.log(evalWtf(complexWtf)) // "a1"