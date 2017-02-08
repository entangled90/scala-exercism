/**
  * Created by carlo on 08/02/17.
  */
object Hamming {

    def compute( first : String, second : String ) = {
        if (first.length == second.length) {
            Some(first.zip(second).foldLeft(0)({
                case (acc,(sx , dx )) if sx != dx =>
                    acc +1
                case (acc,_)=>
                    acc
            }))
        }
        else None
    }
}
