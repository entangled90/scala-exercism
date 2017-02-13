import scala.collection.parallel.mutable.ParArray

object Frequency {



  def frequency(numWorkers: Int, texts: Seq[String]): Map[Char, Int] = frequencyWithParallelCollections(numWorkers,texts)

  def frequencyWithParallelCollections(n : Int, texts : Seq[String]): Map[Char, Int] = ???
}
