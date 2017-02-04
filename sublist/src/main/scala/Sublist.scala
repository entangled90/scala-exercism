import scala.annotation.tailrec

/**
  * Created by carlo on 03/02/17.
  */
object Sublist {


    case object MinusOnEquals extends Exception

    sealed trait Comparison {
        def unary_- : Comparison
    }

    case object Equal extends Comparison {
        override def unary_- = throw MinusOnEquals
    }

    case object Superlist extends Comparison {
        override def unary_- = Sublist
    }

    case object Sublist extends Comparison {
        override def unary_- = Superlist
    }

    case object Unequal extends Comparison {
        override def unary_- = Unequal
    }


    def sublist[A](first: List[A], second: List[A]): Comparison = {

        def isSublist(listToSearch: List[A], targetList: List[A], remainingElementsToMatch: List[A],
                      originalTargetList: List[A], originalListToSearch : List[A]): Comparison = {

            (listToSearch, remainingElementsToMatch) match {
                case (h1 :: t1, h2 :: t2) if h1 == h2 =>
                    isSublist(t1, targetList, t2, originalTargetList,originalListToSearch)

                //Means
                // [1,2] in [1,2,3]
                case (Nil, h :: t) =>
                    println("Sublist by nil")
                    Sublist

                //Could be equals or
                // [1 ,2 ] in [3,1,2]
                case (Nil, Nil) =>
                    if (targetList == originalTargetList) Equal else Sublist

                case (_, _) =>
                    if (targetList.nonEmpty)
                        isSublist(originalListToSearch, targetList.tail, targetList.tail, originalTargetList,originalListToSearch)
                    else
                        Unequal

            }

        }
            isSublist(first, second, second, second,first) match {
                case Unequal =>
                    println("Reverse")
                    -isSublist(second, first, first, first,second)
                case other =>
                    other
            }

    }


}