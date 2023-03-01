# validation-selective

![Logo](https://user-images.githubusercontent.com/8126674/76859713-c0880200-6851-11ea-8bc1-bb9dee87d44f.png)
[![GitHub CI](https://github.com/kowainik/validation-selective/workflows/CI/badge.svg)](https://github.com/kowainik/validation-selective/actions)

[![Hackage](https://img.shields.io/hackage/v/validation-selective.svg?logo=haskell)](https://hackage.haskell.org/package/validation-selective)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](LICENSE)

Lightweight pure data validation based on `Applicative` and `Selective` functors.

`validation-selective` is built around the following data type:

```haskell
data Validation e a
    = Failure e
    | Success a
```

This data type is similar to `Either` but allows accumulating all
errors instead of short-circuiting on the first one.

For more examples and library tutorial, refer to Haddock:

* [`validation`: Official documentation](http://hackage.haskell.org/package/validation-selective/docs/Validation.html)

## Comparison with other packages

`validation-selective` is not the only package that provides such
`Validation` data type. However, unlike other packages, it has some
noticeable advantages:

+ **Lightweight**. `validation-selective` depends only on `base` and
  `selective` (which is tiny) Haskell libraries which make this
  package fast to build. So adding validation capabilities to your
  library or application doesn't contribute much to your dependency
  footprint.
+ **Selective instance.** `validation-selective` is the only package
  that provides `Selective` instance for `Validation` which allows
  using `Monad`-like branching behaviour but without implementing
  wrong `Monad` instance.
+ **More algebraic instances.** `validation-selective` also provides
  the `Alternative` instance and a more general `Semigroup` instance.
+ **Best-in-class documentation.** Official Haddock documentation
  contains mini-tutorial, usage example, per-component comparison with
  `Either`, the motivation behind each instance and the interface in
  general along with examples for **each instance and function**.

The below section provides per-package comparison with the most
popular validation packages in the Haskell ecosystem:

+ [`either`](https://hackage.haskell.org/package/either): `Validation`
  implementation by Edward Kmett. This package is more heavyweight,
  since it depends on more Haskell libraries like `profunctors`,
  `bifunctors`, `semigroupoids`. But it also provides prisms for
  `Validation` and some combinators for `Either`.
+ [`validation`](https://hackage.haskell.org/package/validation):
  `Validation` from [Queensland Functional Programming Lab](https://qfpl.io/).
  Depends on `lens`, which makes it even heavier but also have richer
  interface compared to the `either` package.

## How to use

`validation-selective` is compatible with the latest GHC compiler
versions starting from `8.6`.

In order to start using `validation-selective` in your project, you
will need to set it up with the three easy steps:

1. Add the dependency on `validation-selective` in your project's
   `.cabal` file. For this, you should modify the `build-depends`
   section by adding the name of this library. After the adjustment,
   this section could look like this:

   ```haskell
   build-depends: base ^>= 4.14
                , validation-selective ^>= 0.0
   ```
2. In the module where you wish to implement pure data validation, you
   should add the import:

   ```haskell
   import Validation (Validation (..))
   ```
3. Now you can use the types and functions from the library:

   ```haskell
   main :: IO ()
   main = print [Failure "wrong", Success 42]
   ```

### Usage with Stack

If `validation-selective` is not available on your current Stackage
resolver yet, fear not! You can still use it from Hackage by adding
the following to the `extra-deps` section of your `stack.yaml` file:

```yaml
extra-deps:
  - validation-selective-CURRENT_VERSION
```
