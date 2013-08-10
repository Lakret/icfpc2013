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
/*
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
*/

sealed trait Expr {
  def size: Int = this match {
    case Lambda(vars, expr) => expr.size
    case Zero() => 1
    case One() => 1
    case If0(cond, thenBranch, elseBranch) =>  1 + cond.size + thenBranch.size + elseBranch.size
    case Fold(arg, acc, step) => 2 + arg.size + acc.size + step.size
    case _ => throw new Exception(s"Illegal expression: ${this.toString()}")
  }
}

sealed trait UnaryOp extends Expr { self =>
  val arg: Expr
  override def size = 1 + arg.size
}

sealed trait BinaryOp extends Expr { self =>
  val x, y : Expr
  override def size = 1 + x.size + y.size
}

case class Lambda(vars : List[Expr], expr : Expr) extends Expr
case class Zero() extends Expr
case class One() extends Expr
case class If0(cond : Expr, thenBranch : Expr, elseBranch : Expr) extends Expr
case class Fold(arg : Expr, acc : Expr, step : Lambda) extends Expr
case class Not(arg : Expr) extends UnaryOp
case class Shl1(arg : Expr) extends UnaryOp
case class Shr1(arg : Expr) extends UnaryOp
case class Shr4(arg : Expr) extends UnaryOp
case class Shr16(arg : Expr) extends UnaryOp
case class And(x : Expr, y : Expr) extends BinaryOp
case class Or(x : Expr, y : Expr) extends BinaryOp
case class Xor(x : Expr, y : Expr) extends BinaryOp
case class Plus(x : Expr, y : Expr) extends BinaryOp
case class Id(name : String) extends Expr
