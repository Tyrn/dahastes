# Dahastes a.k.a. Procrustes

[![dahastes on hackage](https://img.shields.io/hackage/v/dahastes)](http://hackage.haskell.org/package/dahastes)
[![dahastes on Stackage Nightly](https://stackage.org/package/dahastes/badge/nightly)](https://stackage.org/nightly/package/dahastes)

Once generated with [template-haskell](https://github.com/jonascarpay/template-haskell); no longer.

## Books

- A&M, notes and exercises

  - [mvaldesdeleon](https://github.com/mvaldesdeleon/haskell-book)
  - [joanllenas](https://github.com/joanllenas/haskell-exercises)
  - [BoeingX](https://github.com/BoeingX/haskell-programming-from-first-principles)
  - [johnchandlerburnham](https://github.com/johnchandlerburnham/hpfp)
  - [glebec](https://github.com/glebec/haskell-programming-allen-moronuki);
    .lhs files used
  - [dwayne](https://github.com/dwayne/haskell-programming)
  - [parry84](https://github.com/parry84/haskell-book)
  - [pushcx](https://github.com/pushcx/hpffp-resources)
  - [gvolpe](https://github.com/gvolpe/haskell-book-exercises)
  - [scarvalhojr](https://github.com/scarvalhojr/haskellbook)
  - [xnning](https://github.com/xnning/haskell-programming-from-first-principles)

## Notes

- [Install tools](https://github.com/Tyrn/dotfiles/blob/main/messy-notes/Haskell.md) (GHCup)

## Howto

- Build and install (as of 2026-08-11 GHC 9.10.3)

```
cabal build
```

```
cabal install --installdir=$HOME/.local/bin --overwrite-policy=always
```

- [Calligraphy](https://github.com/jonascarpay/calligraphy),
  [Notes](https://github.com/Tyrn/dotfiles/blob/main/messy-notes/Haskell.md)

- `htaglib` inquiry (depends on `extra/taglib 2.x`)

```
pkg-config --modversion taglib
```

```
ldd ~/.local/bin/dahastes | grep libtag
```

Should be something like `libtag_c.so.2` for `taglib 2.x`

## TODO

- Decide if checking the audio files by extensions is enough.
  In practice, the broken files are rare and won't go unnoticed
  as soon as the tag setting is attempted.
