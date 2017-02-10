
import scala.util.{Failure, Success, Try}

object RunLengthEncoding {

    object EncodeMatchers {
        def empty = EncodeMatchers(None, "")
    }

    case class EncodeMatchers(previousLetter: Option[(Char, Int)], current: String) {

        def encode = {
            val appendableString = previousLetter.map {
                case (letter, n) if n > 1 => n.toString + letter
                case (letter, 1) => letter
            }.getOrElse("")
            current + appendableString
        }

    }


    object DecodeMatchers{
        def empty = DecodeMatchers(0,"")
    }


    case class DecodeMatchers(repetitions: Int, accumulator: String) {
        def decode(c : Char, n : Int) = {
            val appendableString = (for (_ <- 1 to n) yield c).mkString("")
            accumulator + appendableString
        }
    }


    def encode(str: String): String = {
        str.foldLeft(EncodeMatchers.empty) {
            case (EncodeMatchers(Some((letter, n)), encoded), current) if current == letter =>
                EncodeMatchers(Some((letter, n + 1)), encoded)

            case (m@EncodeMatchers(content, encoded), current) =>
                println(s"Matching $m")
                EncodeMatchers(Some(current, 1), m.encode)
        }.encode
    }

    def decode(str: String): String = {
        str.foldLeft(DecodeMatchers.empty){
            case (d@DecodeMatchers(n, acc), current) =>
                //IMPORTANT TO TELL THIS TO PEOPLE!
                println(s"matchers are $d, current is $current")
                current.asDigit match {
                    case parsed if parsed < 10 && parsed > -1  =>
                        println("Success")
                        DecodeMatchers(n * 10 + parsed, acc)

                    case _ =>
                        println("Failure")
                        val repetitions = if (n == 0) 1 else n
                        DecodeMatchers(0, d.decode(current,repetitions))
                }
        }.accumulator
    }
}
