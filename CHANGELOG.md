# Changelog

`validation-selective` uses [PVP Versioning][1].
The changelog is available [on GitHub][2].

## 0.2.0.0 — Mar 1, 2023

* [#62](https://github.com/kowainik/validation-selective/issues/62):
  Support GHC-9.4.
* Allow `hedgehog-1.2`.
* Allow `selective-0.6`.
* Allow `doctest-0.21`.
* Remove support for GHC-8.4 (due to `selective-0.6`, which dropped support for
  the earlier GHCs).

## 0.1.0.2 — Jun 11, 2021

* [#62](https://github.com/kowainik/validation-selective/issues/62):
  Support GHC-9.2.
* [#64](https://github.com/kowainik/validation-selective/issues/64):
  Allow `selective` version `0.5`.
* Allow `hspec-2.11`.
* Allow `doctest-0.20`.

## 🥧 0.1.0.1 — Mar 14, 2021

* [#57](https://github.com/kowainik/validation-selective/issues/57):
  Support GHC-9.0. Upgrade minor version to 8.10.4 and 8.8.4.

## 0.1.0.0 — May 5, 2020

* [#41](https://github.com/kowainik/validation-selective/issues/41):
  Support GHC-8.10.1.
* [#24](https://github.com/kowainik/validation-selective/issues/24):
  Add `validationAll`, `when*` and `maybe*` combinators into
  `Validation.Combinators`.

## 0.0.0.0

* Initially created.

[1]: https://pvp.haskell.org
[2]: https://github.com/kowainik/validation-selective/releases
