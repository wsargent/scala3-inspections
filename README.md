## Scala 3 Inspection Macro

This is an implementation of a inspection macro in Scala 3, which rewrites the AST to insert logging statements.

There's already an implementation for [Scala 2](https://tersesystems.github.io/blindsight/usage/inspections.html) but Scala 3 macros are different enough that they must be re-implemented from scratch.

Other macro example projects are mostly concerned with types rather than rewriting trees, so this is hopefully breaking new territory for Scala 3.

```scala
InspectionMacros.decorateIfs(dif => logger.debug(s"${dif.code} = ${dif.result}")) {
  if (System.currentTimeMillis() - 1 < 0) {
    println("decorateIfs: if block")
  } else if (System.getProperty("derp") != null) {
    println("decorateIfs: if else block")
  } else {
    println("decorateIfs: else block")
  }
}
```

will stick block of `logger.debug` into each branch, so the end result is:

```scala
if (java.lang.System.currentTimeMillis().-(1).<(0)) {
  output$proxy2.apply(example.BranchInspection.apply("java.lang.System.currentTimeMillis().-(1).<(0)", true))
  scala.Predef.println("decorateIfs: if block")
} else {
  output$proxy2.apply(example.BranchInspection.apply("java.lang.System.currentTimeMillis().-(1).<(0)", false))
  if (java.lang.System.getProperty("derp").!=(null)) {
    output$proxy2.apply(example.BranchInspection.apply("java.lang.System.getProperty(\"derp\").!=(null)", true))
    scala.Predef.println("decorateIfs: if else block")
  } else {
    output$proxy2.apply(example.BranchInspection.apply("java.lang.System.getProperty(\"derp\").!=(null)", false))
    scala.Predef.println("decorateIfs: else block")
  }
}
```