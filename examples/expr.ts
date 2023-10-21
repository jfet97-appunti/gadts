export {}

// data Expr a where
//   N   :: Int  -> Expr Int
//   B   :: Bool -> Expr Bool
//   Add :: Expr Int -> Expr Int -> Expr Int
//   Mul :: Expr Int -> Expr Int -> Expr Int
//   Eq  :: Eq x => Expr x -> Expr x -> Expr Bool

// Yoneda says that:
// (Functor f) => f a ~ (forall r. (a -> r) -> f r)

// --------------------------- Expr<A> ---------------------------

type Expr<A> =
    | Num<A>
    | Bool<A>
    | Add<A>
    | Mul<A>
    | Equal<A>

// --------------------------- Num<A> ---------------------------
// (Functor Expr) => Expr number ~ (forall r. (number -> r) -> Expr r)

interface Num<A> {
    value: number
    eq: (_: number) => A
    _tag: "Num"
}

// this constructor lifts a number into an encoded Expr<number>
function Num_y(n: number) {
    return <R>(eq:(_: number) => R): Expr<R> => ({
        value: n,
        eq,
        _tag: "Num",
    } satisfies Num<R>)
}

// -------------------------- Bool<A> --------------------------
// (Functor Expr) => Expr boolean ~ (forall r. (boolean -> r) -> Expr r)

interface Bool<A> {
    value: boolean
    eq: (_: boolean) => A
    _tag: "Bool"
}

// this constructor lifts a boolean into an encoded Expr<boolean>
function Bool_y(b: boolean) {
    return <R>(eq:(_: boolean) => R): Expr<R> => ({
        value: b,
        eq,
        _tag: "Bool",
    } satisfies Bool<R>)
}

// --------------------------- Add<A> ---------------------------
// (Functor Expr) => Expr number ~ (forall r. (number -> r) -> Expr r)

interface Add<A> {
    en1: Expr<number>
    en2: Expr<number>
    eq: (_: number) => A
    _tag: "Add"
}

// this constructor combines two Expr<number> into an encoded Expr<number> representing their sum
function Add_y(en1: Expr<number>, en2: Expr<number>) {
    return <R>(eq:(_: number) => R): Expr<R> => ({
        en1,
        en2,
        eq,
        _tag: "Add",
    } satisfies Add<R>)
}

// --------------------------- Mul<A> ---------------------------
// (Functor Expr) => Expr number ~ (forall r. (number -> r) -> Expr r)

interface Mul<A> {
    en1: Expr<number>
    en2: Expr<number>
    eq: (_: number) => A
    _tag: "Mul"
}

// this constructor combines two Expr<number> into an encoded Expr<number> representing their multiplication
function Mul_y(en1: Expr<number>, en2: Expr<number>) {
    return <R>(eq:(_: number) => R): Expr<R> => ({
        en1,
        en2,
        eq,
        _tag: "Mul",
    } satisfies Mul<R>)
}

// --------------------------- Equal<A> ---------------------------
// (Functor Expr) => Expr boolean ~ (forall r. (boolean -> r) -> Expr r)

interface EqualU<A, X> {
    ex1: Expr<X>
    ex2: Expr<X>
    eq: (_: boolean) => A
}

interface Equal<A> {
  // ∃X.Equal<A, X> ~ ∀R.(∀X.Equal<A, X> => R) => R
  equal: <R>(_: <X>(_: EqualU<A, X>) => R) => R,
  _tag: "Eq"
}

// this constructor combines two Expr<X> into an encoded Expr<boolean> representing their equality
// note: X has existential nature
function Equal_y<X>(ex1: Expr<X>, ex2: Expr<X>) {
    return <R>(eq:(_: boolean) => R): Expr<R> => ({
        equal: cont => cont({
          ex1,
          ex2,
          eq,
        }),
        _tag: "Eq",
    } satisfies Equal<R>)
}

// ---------------- Functor instance for Expr<A> ----------------

// Functor instance for Expr<A>, mandatory for Yoneda encoding
function fmap<A, B>(f: ( _: A) => B, ea: Expr<A>): Expr<B> {
    switch(ea._tag) {
        case "Num": {
            return Num_y(ea.value)((_) => f(ea.eq(_)))
        }
        case "Bool": {
            return Bool_y(ea.value)((_) => f(ea.eq(_)))
        }
        case "Add": {
            return Add_y(ea.en1, ea.en2)((_) => f(ea.eq(_)))
        }
        case "Mul": {
            return Mul_y(ea.en1, ea.en2)((_) => f(ea.eq(_)))
        }
        case "Eq": {
            return ea.equal(ea => Equal_y(ea.ex1, ea.ex2)((_) => f(ea.eq(_))))
        }
    }
}


// ---------------- Yoneda encoding & decoding ----------------

// (Functor f) => f a -> (forall r. (a -> r) -> f r)
// where f is fixed to Expr for simplicity
function to_yoneda<A>(decoded: Expr<A>): <R>(eq: (_: A) => R) => Expr<R> {
    return eq => fmap(eq, decoded)
}

// (Functor f) => (forall r. (a -> r) -> f r) -> f a
// where f is fixed to Expr for simplicity
function from_yoneda<A>(encoded: <R>(eq: (_: A) => R) => Expr<R>): Expr<A> {
    return encoded(_ => _)
}

// example

const encoded = Num_y(12)
//    ^?
const decoded = from_yoneda(Num_y(12))
//    ^?
const encoded_again = to_yoneda(decoded)
//    ^?

// from_yoneda . to_yoneda = id
// to_yoneda . from_yoneda = id
// the proof is left as an exercise to the reader

// ------------------- helpers -------------------

function num(n: number): Expr<number> {
    return from_yoneda(Num_y(n))
}

function bool(b: boolean): Expr<boolean> {
    return from_yoneda(Bool_y(b))
}

function add(en1: Expr<number>, en2: Expr<number>): Expr<number> {
    return from_yoneda(Add_y(en1, en2))
}

function mul(en1: Expr<number>, en2: Expr<number>): Expr<number> {
    return from_yoneda(Mul_y(en1, en2))
}

function equal<B>(eb1: Expr<B>, eb2: Expr<B>): Expr<boolean> {
    return from_yoneda(Equal_y(eb1, eb2))
}

// ------------------- Eval -------------------

function evalExpr<A>(ea: Expr<A>): A {
  switch(ea._tag) {
        case "Num": {
            return ea.eq(ea.value)
        }
        case "Bool": {
            return ea.eq(ea.value)
        }
        case "Add": {
            return ea.eq(evalExpr(ea.en1) + evalExpr(ea.en2))
        }
        case "Mul": {
            return ea.eq(evalExpr(ea.en1) * evalExpr(ea.en2))
        }
        case "Eq": {
          return ea.equal(ea => ea.eq(evalExpr(ea.ex1) === evalExpr(ea.ex2)))
        }
    }
}

// examples

const exp1 = add(num(1), num(2))

const exp_err = add(bool(false), num(2))

const exp2 = equal(mul(exp1, num(10)), num(30))

const exp3 = equal(bool(false), bool(true))

console.log(({ exp1: evalExpr(exp1), exp2: evalExpr(exp2), exp3: evalExpr(exp3) }))
