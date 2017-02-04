import scala.collection.immutable.Stack

/**
  * Created by carlo on 04/02/17.
  */
object Brackets {

    val leftBrackets = Set('(','[','{')
    val rightBrackets = Set(')',']','}')
    val brackets = (c : Char) => leftBrackets(c) || rightBrackets(c)

    def toTyped(s: Char): Option[Char] =  if (brackets(s)) Some(s) else None


    implicit class bracketsOps( val c : Char) extends AnyVal {
        def isLeft = leftBrackets(c)
        def isRight = rightBrackets(c)

        def mirror = c match {
            case ')' => '('
            case  ']'=> '['
            case '}' => '{'
        }
    }


    def areBalanced(s : String ) = areBalancedWithRecursion(s)

    def areBalancedWithApi(s: String): Boolean = {
        s.flatMap(toTyped).foldLeft((Vector.empty[Char],true)){
            case ((_,false),_) =>
                (Vector.empty,false)

            case ((acc, true ), char) if char.isLeft =>
                (acc :+ char, true)

            case ((acc, true), char) if char.isRight =>
                if (acc.isEmpty)
                    (Vector.empty,false)
                else {
                    val previous = acc.last
                    if ( char.mirror == previous)
                        (acc.init,true)
                    else (Vector.empty,false)
                }
        } match {
            case (Seq(),true) => true
            case _ => false
        }
    }


    def areBalancedWithRecursion(s : String) : Boolean = {

        //List is to be used as a stack, using only :: & init methods
        def check( brackets : List[Char], stack : List[Char]) : Boolean= {
            brackets match {
                case Nil =>
                    stack.isEmpty
                case head :: tail if head.isRight=>
                    if (stack.headOption.contains(head.mirror))
                        check(tail,stack.tail)
                    else false
                case head :: tail if head.isLeft =>
                    check(tail, head :: stack )
            }
        }
        check(s.flatMap(toTyped).toList,Nil)

    }

}
