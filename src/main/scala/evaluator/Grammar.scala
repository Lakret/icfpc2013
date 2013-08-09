package evaluator

// program    P ::= "(" "lambda" "(" id ")" e ")"
//  expression e ::= "0" | "1" | id
//                | "(" "if0" e e e ")"
//                | "(" "fold" e e "(" "lambda" "(" id id ")" e ")" ")"
//                | "(" op1 e ")"
//                | "(" op2 e e ")"
//           op1 ::= "not" | "shl1" | "shr1" | "shr4" | "shr16"
//           op2 ::= "and" | "or" | "xor" | "plus"
//           id  ::= [a-z][a-z_0-9]*

// A valid program P contains at most one occurrence of "fold".
// The only constants in a source program are 0 and 1.
// However, \BV programs can be evaluated on arbitrary 64-bit values.

sealed trait BV {
  def isUnaryOp : Boolean =
    this match {
      case Not(_) | Shl1(_) | Shr1(_) | Shr4(_) | Shr16(_) =>
        true
      case _ => false
    }

  def unaryOpArg : BV =
    this match {
      case Not(x) => x
      case Shl1(x) => x
      case Shr1(x) => x
      case Shr4(x) => x
      case Shr16(x) => x
      case _ =>
        throw new IllegalArgumentException("%s is not an unary op" format(this))
    }

  def isBinaryOp : Boolean =
    this match {
      case And(_, _) | Or(_, _) | Xor(_, _) | Plus(_, _) =>
        true
      case _ => false
    }

  def binaryOpArgs : List[BV] =
    this match {
      case And(x, y)  => List(x, y)
      case Or(x, y)   => List(x, y)
      case Xor(x, y)  => List(x, y)
      case Plus(x, y) => List(x, y)
      case _ =>
        throw new IllegalArgumentException("%s is not a binary op" format(this))
    }
}

case class Lambda(vars : List[BV], expr : BV) extends BV
case class Zero() extends BV
case class One() extends BV
case class If0(cond : BV, thenBranch : BV, elseBranch : BV) extends BV
case class Fold(arg : BV, acc : BV, step : Lambda) extends BV
case class Not(arg : BV) extends BV
case class Shl1(arg : BV) extends BV
case class Shr1(arg : BV) extends BV
case class Shr4(arg : BV) extends BV
case class Shr16(arg : BV) extends BV
case class And(x : BV, y : BV) extends BV
case class Or(x : BV, y : BV) extends BV
case class Xor(x : BV, y : BV) extends BV
case class Plus(x : BV, y : BV) extends BV
case class Id(name : String) extends BV
