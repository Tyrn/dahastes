# dahastes

[![dahastes on hackage](https://img.shields.io/hackage/v/dahastes)](http://hackage.haskell.org/package/dahastes)
[![dahastes on Stackage Nightly](https://stackage.org/package/dahastes/badge/nightly)](https://stackage.org/nightly/package/dahastes)

Generated with [template-haskell](https://github.com/jonascarpay/template-haskell)

- [Install tools](https://github.com/Tyrn/dotfiles/blob/main/messy-notes/Haskell.md) (GHCup)

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
