Sturgeon — A toolkit for communicating with Emacs
-------------------------------------------------------------------------------
%%VERSION%%

Sturgeon implements various tools for manipulating Emacs from OCaml:
- `Sturgeon_sexp` implements the Emacs dialect of S-expressions
- `Sturgeon_session` implements an "session protocol" to make RPC to Emacs from OCaml and vice versa
- `Sturgeon_stui` is a session implementing an [Inuit](https://github.com/let-def/inuit) backend: one can now runs text user-interface on an Emacs buffer
- `Sturgeon_recipes_*` offers different "rendez-vous" points to connect to Emacs

Sturgeon is distributed under the ISC license.

Homepage: https://github.com/let-def/sturgeon  
Contact: Frédéric Bour `<frederic.bour@lakaban.net>`

## Installation

sturgeon can be installed with `opam`:

    opam install sturgeon

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Documentation

The documentation and API reference is automatically generated by
`ocamldoc` from the interfaces. It can be consulted [online][doc]
and there is a generated version in the `doc` directory of the
distribution.

[doc]: https://let-def.github.io/sturgeon/doc

## Sample programs

If you installed sturgeon with `opam` sample programs are located in
the directory `opam config var sturgeon:doc`.

In the distribution sample programs and tests are located in the
[`test`](test) directory of the distribution. They can be built with:

    ocamlbuild -use-ocamlfind test/tests.otarget

The resulting binaries are in `_build/test`.

- `test.native` tests the library, nothing should fail.
