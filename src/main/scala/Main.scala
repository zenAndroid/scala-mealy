import mealy.model.*
import scala.util.Failure
import scala.util.Success

@main def hello: Unit =

  val mach2 = getNDMachine
  mach2 setInputSequence "abababababababababababababab"
  // println(mach2.toDot)
  val consuVal = mach2.nonDeterministicConsume
  println(consuVal(0))
  println(consuVal(1))
