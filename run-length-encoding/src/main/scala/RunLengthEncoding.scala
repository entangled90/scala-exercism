import scala.util.{Failure, Success, Try}

object RunLengthEncoding {

    object EncodeMatchers {
        def empty = EncodeMatchers(None, "")
    }

    case class EncodeMatchers(previousLetter: Option[(Char, Int)],
                              current: String)

    def encode(matchers: EncodeMatchers): String = {
        val appendableString = matchers.previousLetter
          .map {
              case (letter, n) if n > 1 => n.toString + letter
              case (letter, 1) => letter
          }
          .getOrElse("")
        matchers.current + appendableString
    }

    object DecodeMatchers {
        def empty = DecodeMatchers(0, "")
    }

    case class DecodeMatchers(repetitions: Int, accumulator: String)

    def decode(c: Char, matcher: DecodeMatchers) = {
        val appendableString =
            (for (_ <- 1 to matcher.repetitions) yield c).mkString("")
        matcher.accumulator + appendableString
    }

    def encode(str: String): String = {
        val lastMatcher = str.foldLeft(EncodeMatchers.empty) {
            case (EncodeMatchers(Some((letter, n)), encoded), current)
                if current == letter =>
                EncodeMatchers(Some((letter, n + 1)), encoded)

            case (m@EncodeMatchers(content, encoded), current) =>
                println(s"Matching $m")
                EncodeMatchers(Some(current, 1), encode(m))
        }
        encode(lastMatcher)
    }

    def decode(str: String): String = {
        str.foldLeft(DecodeMatchers.empty) {
              case (d@DecodeMatchers(n, acc), current) =>
                  //IMPORTANT TO TELL THIS TO PEOPLE!
                  current.asDigit match {
                      case parsed if parsed < 10 && parsed > -1 =>
                          DecodeMatchers(n * 10 + parsed, acc)

                      case _ =>
                          val repetitions = if (n == 0) 1 else n
                          DecodeMatchers(
                              0,
                              decode(current, d.copy(repetitions = repetitions)))
                  }
          }.accumulator
    }
}
