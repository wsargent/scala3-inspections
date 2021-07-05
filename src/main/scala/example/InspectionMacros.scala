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

  object Impl {

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

