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

sealed trait BV

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
