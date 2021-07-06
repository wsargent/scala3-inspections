package example

/**
 * Debugs a branch (if / match).
 *
 * @param code the condition of the branch
 * @param result the result of evaluating the condition
 */
case class BranchInspection(code: String, result: Boolean)

/**
 * Debugs a result.
 *
 * @param code the code that went into the result
 * @param value the result
 * @tparam A the type of the result.
 */
case class ExprInspection[A](code: String, value: A)

/**
 * Debugs a valdef.
 *
 * @param name name of the val or var
 * @param value the value of the val or var
 */
case class ValDefInspection(name: String, value: Any)

object InspectionMacros {
  import scala.quoted.*

  inline def decorateIfs[A](output: BranchInspection => Unit)(ifStatement: => A): A =
    ${ Impl.decorateIfsImpl('output, 'ifStatement)}

  inline def decorateMatch[A](output: BranchInspection => Unit)(matchStatement: => A): A =
    ${ Impl.decorateMatchImpl('output, 'matchStatement) }

  inline def dumpExpression[A](block: A): ExprInspection[A] =
    ${ Impl.dumpExpressionImpl('block) }

  inline def decorateVals[A](output: ValDefInspection => Unit)(block: => A): A =
    ${ Impl.decorateValsImpl('output, 'block) }

  object Impl {

    def decorateMatchImpl[A: Type](
                                    output: Expr[BranchInspection => Unit],
                                    matchStatement: Expr[A]
                                  )(using Quotes): Expr[A] = {
      import quotes.reflect.*

      matchStatement.asTerm match {
        case Inlined(_, _, matchIdent: Ident) =>
          matchIdent.symbol.tree match {
            case DefDef(_, _, _, Some(term)) =>
              // Block(List(),Match(Ident(string),List(CaseDef(Bind(s,Ident(_)),Apply(Select(Ident(s),startsWith),List(Literal(Constant(20)))),Block(List(),Apply(Ident(println),List(Literal(Constant(this example is still valid)))))), CaseDef(Ident(_),EmptyTree,Block(List(),Apply(Ident(println),List(Literal(Constant(oh dear)))))))))
              //println(s"matchStatement = ${term}")
              val modifiedMatch = term match {
                case Block(b, Match(m, cases)) =>
                  // List of case defs
                  //println(s"cases = ${cases}")
                  val enhancedCases = cases.map {
                    case CaseDef(pat: Tree, guard: Option[Term], body: Term) =>
                      val guardSource = guard.map(t => s" if ${t.show}").getOrElse("")
                      val patSource   = pat match {
                        case Bind(b, f) => s"case $b"
                        case Ident(i) => s"case $i"
                        case other =>
                          println(s"Unexpected macro case $other")
                          s"case $other"
                      }
                      val src         = Expr(s"$patSource$guardSource")
                      val bodyExpr    = body.asExpr
                      val stmt        = '{ $output(BranchInspection($src, true)); $bodyExpr }
                      CaseDef(pat, guard, stmt.asTerm)
                  }
                  Block(b, Match(m, enhancedCases))
                case other =>
                  println(s"other = $other")
                  other
              }
              //println(s"${modifiedMatch.show}")
              modifiedMatch.asExpr.asInstanceOf[Expr[A]]

            case otherIdent =>
              report.error(s"Not a valid identifier: ${otherIdent}")
              matchStatement
          }
        case other =>
          report.error(s"Parameter must be a known ident: ${other.show}")
          matchStatement
      }
    }

    def decorateIfsImpl[A: Type](
      output: Expr[BranchInspection => Unit],
      ifStatement: Expr[A]
    )(using Quotes): Expr[A] = {
      import quotes.reflect.*

      def findIfMethod(ifTerm: Term): Expr[A] = {
        ifTerm match {
          case Block(stats, If(condTerm, thenTerm, elseTerm)) =>
            constructIf(condTerm, thenTerm, elseTerm)

          case If(condTerm, thenTerm, elseTerm) =>
            constructIf(condTerm, thenTerm, elseTerm)

          case other =>
            //println(s"Rendering block as $other")
            ifTerm.asExpr.asInstanceOf[Expr[A]]
        }
      }

      def constructIf(condTerm: Term, thenTerm: Term, elseTerm: Term): Expr[A] = {
        val condSource = condTerm.show
        val branchTrue = '{ BranchInspection(${Expr(condSource)}, true) }
        val branchFalse = '{ BranchInspection(${Expr(condSource)}, false) }
        val cond: Expr[Boolean] = condTerm.asExpr.asInstanceOf[Expr[Boolean]]
        val thenp: Expr[A] = thenTerm.asExpr.asInstanceOf[Expr[A]]
        val elsep: Expr[A] = findIfMethod(elseTerm)

        // Return a construction with the new statement
        val remade = '{ if ($cond) { $output($branchTrue); $thenp } else { $output($branchFalse); $elsep } }
        //println(s"remade = ${remade.show}")
        remade
      }

      // What we get is an inlined ident pointing to the method.
      // so we have to dig a little to get the actual if statement.
      ifStatement.asTerm match {
        case Inlined(_, _, ifIdent: Ident) =>
          // XXX blockTerm.symbol
          ifIdent.symbol.tree match {
            case DefDef(_, _, _, Some(term)) =>
              findIfMethod(term)

            case otherIdent =>
              report.error(s"Not a valid identifier: ${otherIdent}")
              ifStatement
          }
        case other =>
          report.error(s"Parameter must be a known ident: ${other.show}")
          ifStatement
      }
    }

    def dumpExpressionImpl[A: Type](block: Expr[A])(using Quotes): Expr[ExprInspection[A]] = {
      import quotes.reflect.*

      // this is the same as sourcecode.Text :-/
      val result = block.asTerm.pos.sourceCode.get
      val const  = Expr(result)
      '{ ExprInspection($const, $block) }
    }

    def decorateValsImpl[A: Type](output: Expr[ValDefInspection => Unit], block: Expr[A])(using Quotes): Expr[A] = {
      import quotes.reflect.*

      // https://melgenek.github.io/scala-3-dynamodb
      //      class MyTreeMap extends TreeMap {
      //        override def transformTree(tree: Tree)(owner: quotes.reflect.Symbol): Tree = {
      //          tree match {
      //            case valdef@ValDef(_, termName, _, _) =>
      //              List(
      //                valdef,
      //                '{ $output(ValDefInspection(${termName.encodedName.toString}, $termName)) }
      //              )
      //            case other => transformTree(other)(owner)
      //          }
      //        }
      //      }
      //      // https://docs.scala-lang.org/scala3/guides/macros/reflection.html
      //      val treeMap = new MyTreeMap
      //      val blockTerm: Term = block.asTerm
      //      treeMap.transformTree(blockTerm)(Symbol.spliceOwner)

      block

      //
      //      val loggedStats = block.asTerm.symbol.tree.flatMap {
      //        case valdef @ ValDef(_, termName, _, _) =>
      //          List(
      //            valdef,
      //            '{ $output(ValDefInspection(${termName.encodedName.toString}, $termName)) }
      //          )
      //        case stat =>
      //          List(stat)
      //      }
      //      val outputExpr: Expr[A] = '{ $loggedStats }
      //      outputExpr
    }
  }

}

