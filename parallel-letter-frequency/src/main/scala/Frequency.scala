import scala.collection.parallel.mutable.ParArray

object Frequency {
  def frequency(numWorkers: Int, texts: Seq[String]): Map[Char, Int] = frequencyWithParallelCollections(numWorkers,texts)



    // Bad "way" to convert an integer < 10 to a char
    val numbers: Set[Char] = (0 to 9).map(x => ( x + '0').toChar).toSet[Char]
    val invalidChars: Set[Char] = Set(',','.',';',':',' ') ++ numbers

  def filterAndLowerCase(text : ParArray[String]) : ParArray[Char] = text.flatMap(_.toLowerCase)
    .filterNot(invalidChars)


  def frequencyWithParallelCollections(n : Int, texts : Seq[String]): Map[Char, Int] = {
    filterAndLowerCase(texts.toParArray).groupBy(identity).foldLeft(Map.empty[Char,Int]){
      case (acc , (key,value)) =>
            acc + ( key -> value.length)
    }
  }
}
