## Scala 3 Inspection Macro

This is an implementation of a inspection macro in Scala 3, which rewrites the AST to insert logging statements.

There's already an implementation for [Scala 2](https://tersesystems.github.io/blindsight/usage/inspections.html) but Scala 3 macros are different enough that they must be re-implemented from scratch.

Other macro example projects are mostly concerned with types rather than rewriting trees, so this is hopefully breaking new territory for Scala 3.

```scala
InspectionMacros.decorateIfs(dif => logger.debug(s"${dif.code} = ${dif.result}")) {
  if (System.currentTimeMillis() - 1 < 0) {
    assert("decorateIfs: if block" != null)
  } else {
    assert("decorateIfs: else block" != null)
  }
}
```

will stick block of `logger.debug` into each branch, so the end result is:

```scala
if (System.currentTimeMillis() - 1 < 0) {
  logger.debug(s"java.lang.System.currentTimeMillis().-(1).<(0) = true")
  assert("decorateIfs: if block" != null)
} else {
  logger.debug(s"java.lang.System.currentTimeMillis().-(1).<(0) = false")
  assert("decorateIfs: else block" != null)
}
```