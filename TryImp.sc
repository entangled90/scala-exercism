import scala.util.control.NonFatal

object Try {
    def apply[T](value : => T)  : Try[T] = {
        try {
            Success(value)
        } catch catcher[T]
    }


    def catcher[T] : PartialFunction[Exception,Error[T]] = {
        case e : Exception =>
            Error(e)
    }
}



sealed abstract class Try[T] {
    def map[V](f: T => V): Try[V]
    def flatMap[V](f : T => Try[V]) : Try[V]
}


case class Success[T](value : T) extends Try[T] {
    //TODO Modify
    override def map[V](f: (T) => V): Try[V] = Try(f(value))

    override def flatMap[V](f: (T) => Try[V]): Try[V] =
        try f(value)
        catch Try.catcher[V]
}



case class Error[T](error : Exception) extends Try[T] {
    override def map[S](f: (T) => S): Try[S] = this.asInstanceOf[Error[S]]

    override def flatMap[V](f: (T) => Try[V]) = Error(error)
}


Success(5).map { x =>
    throw new NullPointerException
    x
}

