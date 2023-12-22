# h10: A Haskell 2010 type checker

h10 is is a type checker for [Haskell 2010][1]. At least for now, it's a
learning project; it serves no practical purpose.

In addition to a command-line tool, h10 compiles to Wasm and comes with a web
interface. The web interface will soon be served in this repo as a GitHub page.

## Project goals and non-goals

Initially this project will implement most of the Haskell 2010 type system.
100% Haskell 2010 compatibility is *not* a goal. For example, monomorphism
restriction and defaulting (H2010 chapter 4.5.5) are currently not implemented.
We may choose to not implement them and report ambiguous types as type errors.

Once the basics are done and working as expected (with good test coverage), I'm
hoping to implement some of the type system features from GHC:

- Scoped type variables
- Type applications
- Multi-parameter type classes
- Flexible instances and contexts
- Unlifted types (such as `Int#`, unboxed arrays etc.) and levity polymorphism
- Polymorphic kinds and kind signatures
- GADTs
- Rank-N types

h10 will never have any feature flags, everything will be enabled always.

At some point (probably before any of the GHC extensions listed above) I'm
planning to diverge from Haskell 2010 and GHC Haskell, with features like:

- Or patterns
- Row polymorphic sums and products

If I still have the time after these, I'd also like to experiment with
incremental type checking and implementing some of the language server protocol
functions.

## Usage

- `cargo test` runs tests.

- `cargo run` type checks `Prelude.hs` and prints inferred types of top-level
  definitions.

- `./build` compiles the project to Wasm. After this step start a web server in
  the `site` directory and navigate to the served address for the web
  interface.

  The web interface shows all compiler outputs in the page. Panics are only
  shown in the JS console.

[1]: https://www.haskell.org/onlinereport/haskell2010/
