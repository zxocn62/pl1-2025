/** Homework 02
  *
  * Note: For some tasks, test examples are already provided. Be sure to provide
  * tests for all tasks and check your solution with them. From now on, the
  * tasks will not explicitly require tests any more, but I advise you to
  * nevertheless use tests for all programming tasks.
  */

/** Task 1: Visitors (1 subtask)
  */
object Hw02Task1 {

  /** Consider the definition for the count visitor and the print visitor for AE
    * from the lecture (see
    * https://ps-tuebingen-courses.github.io/pl1-lecture-notes/03-arithmetic-expressions/arithmetic-expressions.html).
    */
  case class Visitor[T](num: Int => T, add: (T, T) => T)

  enum Exp:
    case Num(n: Int)
    case Add(lhs: Exp, rhs: Exp)
  import Exp._

  // Fold using visitors
  def foldExp[T](v: Visitor[T], e: Exp): T =
    e match {
      case Num(n)    => v.num(n)
      case Add(l, r) => v.add(foldExp(v, l), foldExp(v, r))
    }

  val countVisitor = Visitor[Int](_ => 1, _ + _)
  val printVisitor = Visitor[String](_.toString, "(" + _ + "+" + _ + ")")

  /** Subtasks:
    *
    * 1) Translate `countVisitor` and `printVisitor` to a definition using
    * pattern matching. Example: Translating the `eval` visitor in this way
    * leads to the `eval` method for object AE from the lecture.
    * (https://ps-tuebingen-courses.github.io/pl1-lecture-notes/04-desugaring/desugaring.html)
    */
}
  // Count using pattern matching
  def count(e: Exp): Int = e match {
    case Num(_)    => 1
    case Add(l, r) => count(l) + count(r)
  }

  // Print using pattern matching
  def print(e: Exp): String = e match {
    case Num(n)    => n.toString
    case Add(l, r) => "(" + print(l) + "+" + print(r) + ")"
  }
  // Test example
  val example = Add(Num(1), Add(Num(2), Num(3)))

  def run(): Unit = {
    println(s"Count result: ${count(example)}")         // 3
    println(s"Print result: ${print(example)}")         // (1+(2+3))
    assert(count(example) == 3)
    assert(print(example) == "(1+(2+3))")
  }



/** Task 2: Desugaring to Nand (1 subtask)
  */
import scala.language.implicitConversions

object Hw02Task2 {

  /** Consider again the language of propositional logic formulae from the
    * previous homework:
    */
  enum Exp:
    case True() // constant true
    case False() // constant false
    case And(lhs: Exp, rhs: Exp)
    case Or(lhs: Exp, rhs: Exp)
    case Not(e: Exp)
    case Impl(lhs: Exp, rhs: Exp)
  import Exp._
 
  object Sugar {
    def Not(e: Exp): Exp = Nand(e, e)
    def And(lhs: Exp, rhs: Exp): Exp = Not(Nand(lhs, rhs))
    def Or(lhs: Exp, rhs: Exp): Exp = Nand(Not(lhs), Not(rhs))
    def Impl(lhs: Exp, rhs: Exp): Exp = Or(Not(lhs), rhs)
  }

 
  def eval(e: Exp): Boolean = e match {
    case True()  => true
    case False() => false
    case Nand(l, r) => !(eval(l) && eval(r))
  }
 
  def run(): Unit = {
    import Sugar._

    val e1 = And(True(), False())        // false
    val e2 = Or(True(), False())         // true
    val e3 = Impl(True(), False())       // false
    val e4 = Not(False())                // true
	
    println(s"Eval And(True, False): ${eval(e1)}")
    println(s"Eval Or(True, False): ${eval(e2)}")
    println(s"Eval Impl(True, False): ${eval(e3)}")
    println(s"Eval Not(False): ${eval(e4)}")

    assert(eval(e1) == false)
    assert(eval(e2) == true)
    assert(eval(e3) == false)
    assert(eval(e4) == true)
  }
  
 object Main {
  def main(args: Array[String]): Unit = {
    Hw02Task1.run()
    Hw02Task2.run()
  }
 
  /** Subtasks:
    *
    * 1) Introduce a new kind of expression `Nand` (not both ... and ...).
    * Eliminate `And`, `Or`, `Not`, and `Impl` by defining them as syntactic
    * sugar for `Nand`. Hint: Constructs defined as syntactic sugar should not
    * occur in the interpreter `eval` anymore.
    */
}
