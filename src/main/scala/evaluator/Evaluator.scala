package evaluator

package object evaluator {
  //TODO
  def validate(expr : BV) = throw new UnsupportedOperationException

  def emptyBindings = Map[String, BigInt]()

  def eval(expr : BV)(bindingsTable : Map[String, BigInt])(input : BigInt) : BigInt =
    expr match {
      case Lambda(Id(x) :: Nil, inner) =>
        evalInner(inner)(bindingsTable + ((x, input)))
      case _ =>
        throw new IllegalArgumentException("top level expression must has form `lambda(x)(e)`")
    }

  def evalInner(expr : BV)(implicit bindingsTable : Map[String, BigInt]) : BigInt =
    expr match {
      case Zero() => 0
      case One() => 1
      case Not(x) => ~ evalInner(x)
      case Shl1(x) => evalInner(x) << 1
      case Shr1(x) => evalInner(x) >> 1
      case Shr4(x) => evalInner(x) >> 4
      case Shr16(x) => evalInner(x) >> 16
      case And(x, y) => evalInner(x) & evalInner(y)
      case Or(x, y) => evalInner(x) | evalInner(y)
      case Xor(x, y) => evalInner(x) ^ evalInner(y)
      case Plus(x, y) => evalInner(x) + evalInner(y)
      case If0(cond, thenBranch, elseBranch) =>
        if (evalInner(cond) == 0) evalInner(thenBranch)
        else evalInner(elseBranch)
      case Fold(init, currAcc, Lambda(Id(x) :: Id(acc) :: Nil, expr)) =>
        (evalInner(init).toString(2).reverse.grouped(8).toList.foldLeft(evalInner(currAcc)){
          case (accv, _v) => {
            val v = BigInt(_v, 2)
            //println("accv = %s, v = %s" format(accv, v))
            evalInner(expr)(bindingsTable -- List(x, acc) ++ Map[String, BigInt](
              x -> v,
              acc -> accv))
          }
        })
      case Id(name) =>
        if (bindingsTable.contains(name)) {
          //println("[%s]=%s" format(name, bindingsTable(name)))
          bindingsTable(name)
        }
        else throw new IllegalArgumentException("There is no instance of %s in bindings table %s" format(name, bindingsTable))
      case _ =>
        throw new UnsupportedOperationException("malformed expression: %s" format expr.toString)
    }

    //evaluator.eval(Lambda(List(Id("x")), Fold(Id("x"), Zero(), Lambda(List(Id("y"), Id("z")), Or(Id("y"), Id("z"))))))(Evaluator.emptyBindings)(BigInt("1" * 64, 2))
}
