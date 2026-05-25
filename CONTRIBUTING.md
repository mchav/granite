# Contributing to granite

## Build & test

```sh
cabal build all
cabal test
```

Golden tests live under `test/golden/`. When a change intentionally alters
rendered output, review the diff and then re-bless:

```sh
GRANITE_BLESS_GOLDEN=1 cabal test
```

## Tutorial & docs

The tutorial is a **[scripths](https://github.com/mchav/scripths) notebook**.
The source of truth is `docs/base/tutorial.md`; the rendered companion
`docs/tutorial.md` is **generated** — do not edit it by hand.

Workflow:

1. **Edit `docs/base/tutorial.md`.** It is prose plus runnable ```haskell``` cells.
   Each cell declares its dependencies inline (`-- cabal: build-depends: ...`),
   builds a chart, and ends with a statement that prints it as SVG, e.g.
   `Text.IO.putStrLn (renderChartSvg chart)`. Cells are GHCi statements (top-level
   bindings and a trailing expression), **not** a `module Main`/`main` program.
2. **Run scripths to generate the final tutorial** — from inside the repo, so cells
   build against the local working tree (scripths auto-detects the enclosing
   `granite` package):

   ```sh
   scripths -o docs/tutorial.md docs/base/tutorial.md
   ```

   scripths evaluates each cell in order and inlines its captured output (the SVG)
   beneath the fence as a block quote.
3. **Commit both** `docs/base/tutorial.md` and the regenerated `docs/tutorial.md`.

> scripths is a separate CLI; install it and ensure it is on your `PATH`.
> (This replaces the older `cabal run granite-tutorial` flow.)
