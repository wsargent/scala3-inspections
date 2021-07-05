package example

/**
 * Debugs a branch (if / match).
 *
 * @param code the condition of the branch
 * @param result the result of evaluating the condition
 */
case class BranchInspection(code: String, result: Boolean)

object InspectionMacros {
  import scala.quoted.*

  inline def decorateIfs[A](output: BranchInspection => Unit)(ifStatement: => A): A =
    ${ Impl.decorateIfsImpl('output, 'ifStatement)}

  inline def decorateMatch[A](output: BranchInspection => Unit)(matchStatement: => A): A =
    ${ Impl.decorateMatchImpl('output, 'matchStatement) }

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
              println(s"${modifiedMatch.show}")
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
  }

}

