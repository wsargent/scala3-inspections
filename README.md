## Scala 3 Inspection Macro

This is an implementation of a inspection macro in Scala 3, which rewrites the AST to insert logging statements.

There's already an implementation for [Scala 2](https://tersesystems.github.io/blindsight/usage/inspections.html) but Scala 3 macros are different enough that they must be re-implemented from scratch.

Other macro example projects are mostly concerned with types rather than rewriting trees, so this is hopefully breaking new territory for Scala 3.