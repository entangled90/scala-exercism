import scala.annotation.tailrec

/**
  * Created by carlo on 03/02/17.
  */
object Sublist {

    sealed trait Comparison

    case object Equal extends Comparison
    case object Superlist extends Comparison
    case object Sublist extends Comparison
    case object Unequal extends Comparison

    def sublist[A](first: List[A], second: List[A]): Comparison = sublistByRecursion(first, second)

    def sublistByRecursion[A](first: List[A], second: List[A]): Comparison = ???
}
