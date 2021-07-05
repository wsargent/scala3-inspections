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

      // What we get is an inlined ident pointing to the method.
      // so we have to dig a little to get the actual if statement.
      ifStatement.asTerm match {
        case Inlined(_, _, ifIdent: Ident) =>
          ifIdent.symbol.tree match {
            case DefDef(_, _, _, Some(Block(_, ifTree))) =>
              ifTree match {
                case If(condTerm, thenTerm, elseTerm) =>
                  val condSource = condTerm.show
                  val branchTrue = '{ BranchInspection(${Expr(condSource)}, true) }
                  val branchFalse = '{ BranchInspection(${Expr(condSource)}, false) }
                  val cond: Expr[Boolean] = condTerm.asExpr.asInstanceOf[Expr[Boolean]]
                  val thenp: Expr[A] = thenTerm.asExpr.asInstanceOf[Expr[A]]

                  // XXX The else statement has to be broken down into if/then recursively...
                  val elsep: Expr[A] = elseTerm.asExpr.asInstanceOf[Expr[A]]

                  // Return a construction with the new statement
                  '{ if ($cond) { $output($branchTrue); $thenp } else { $branchFalse; $elsep } }
                case otherIf =>
                  report.error(s"Not an if statement: ${otherIf}")
                  ifStatement
              }
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

