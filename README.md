<!-- <script
  src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
  type="text/javascript">
</script> -->


# Constrained Tildes in Haskell

## Haskell Installation
The easiest way to install the Haskell tool chain is to use [`ghcup`](https://www.haskell.org/ghcup/).

## PNG Generation
The computed automata can be converted to PNG format using [`graphviz`](https://graphviz.org/download/).

## Computation of the Partial Derivatives
First, run the `stack ghci Test.hs` command in the `src` folder.
This may take some time during the first launch: Haskell packages have to be downloaded and built.

Once the REPL launched, few examples are defined:
- the value `phi` is the Boolean formula $ \phi = (2 \vee 0) \wedge \neg 1 $ over the ternary alphabet $ \{1, 2, 3\}$, defined by
```haskell
phi :: BoolForm (Finite 3)
phi = And (Or (Atom (finite 2)) (Atom (finite 0))) (Not (Atom (finite 1)))
```
- the expression `e` is the constrained tilde $ \phi(a, b, c) $ defined by
```haskell
e :: Exp Char
e = ConsTilde phi $ fromTuple (Symbol 'a', Symbol 'b', Symbol 'c')
```
- the set of derived terms of $ e $ is `res` computed as follows
```haskell
res :: Set (Exp Char)
res = allDeriveBySymbs (S.fromList "abc") e
```
- the function `mirror` can build the mirror operator from a list with an even length:
```haskell
phi' :: BoolForm (Finite 4)
phi' = fromJust $ mirror [Atom 0, Atom 1, Atom 2, Atom 3]
```
- the function `plus` is an alias for the catenation of an expression with its starred version
```haskell
plus :: a -> Exp a
plus a = Symbol a `Concat` Star (Symbol a)
```
- the expression $ \mathrm{Mirror}(a^+, b^+, c^+, d^+) $ can be defined by
```haskell
expr :: Exp Char
expr = ConsTilde phi' $ fromTuple (plus 'a', plus 'b', plus 'c', plus 'd')
```
- the Antimirov automaton can be computed via the function `antimirov`:
```haskell
auto :: NFA (Exp Char) Char
auto = antimirov expr
```
- the conversion to PNG can be done via the function `faToPng`, with the name of the generated file in first parameter (the following example produces a file `test.png`)
```haskell
vizPng :: IO FilePath
vizPng = faToPng "test" auto
```
- parsing a string can be done with the function `expFromString`
```haskell
expr2 :: Exp Char
expr2 = fromJust $ expFromString "|((0&3) Or (Not 0 & Not 3)) And ((1 & 2) Or (Not 1 & Not 2))|(a.(a*), b.(b*), a.(a*), b.(b*))"

vizPng2 :: IO FilePath
vizPng2 = faToPng "test2" $ antimirov expr2
```
