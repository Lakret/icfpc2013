package backtracking

import scala.collection.immutable.Stack

trait Backtracker[Node] {
  //reject a candidate not worth completing
  def reject(x : Node) : Boolean

  def isGoal(x : Node) : Boolean

  //find all possible succesors of x
  def next(x : Node) : Stream[Node]

  //lazily find all possible solutions
  def find(start : Node, stack : Stack[Node] = Stack()) : Stream[Node] = {
    if (reject(start)) {
      Stream[Node]()
    } else {
    }
  }
}
