export {}

// data Wtf a where
//   Nul :: Wtf a
//   WtfInt :: Int -> Wtf Int
//   WtfSimpleEx :: a -> Wtf a
//   WtfComplexEx :: a -> b -> c -> Wtf (a, b)

type Wtf<A> =
  | Nul<A>

  // --------------------------- Nul<A> ---------------------------
// (Functor Wtf) => Wtf a ~ (forall r. (a -> r) -> Wtf r)

type Nul<A> = {
    eq: (_: A) => A
    _tag: "Nul"
}

function Nul_y() {
    return <R>(eq:(_: R) => R): Wtf<R> => ({
        eq,
        _tag: "Nul",
    } satisfies Nul<R>)
}