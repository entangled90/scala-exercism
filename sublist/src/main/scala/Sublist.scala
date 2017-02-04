import scala.annotation.tailrec

/**
  * Created by carlo on 03/02/17.
  */
object Sublist {

    sealed trait Comparison {
        def unary_- : Comparison
    }

    case object Equal extends Comparison {
        override def unary_- = Unequal
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

    def sublist[A](first: List[A], second: List[A]): Comparison = sublistByRecursion(first, second)

    def sublistByRecursion[A](first: List[A], second: List[A]): Comparison = {

        def isSublist(f: List[A], s: List[A]) = {
            @tailrec
            def isSublistImp(listToSearch: List[A],
                             targetList: List[A],
                             remainingElementsToMatch: List[A]): Comparison = {

                (listToSearch, remainingElementsToMatch) match {
                    case (h1 :: t1, h2 :: t2) if h1 == h2 =>
                        isSublistImp(t1,
                            targetList,
                            t2)

                    //Means
                    // [1,2] in [1,2,3]
                    case (Nil, h :: t) =>
                        Sublist

                    //Could be equals or
                    // [1 ,2 ] in [3,1,2]
                    case (Nil, Nil) =>
                        if (targetList == s) Equal else Sublist

                    case (_, _) =>
                        if (targetList.nonEmpty)
                            isSublistImp(f,
                                targetList.tail,
                                targetList.tail)
                        else
                            Unequal

                }

            }

            isSublistImp(f, s, s)
        }

        isSublist(first, second) match {
            case Unequal =>
                - isSublist(second, first)
            case other =>
                other
        }

    }

}
