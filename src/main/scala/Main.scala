import mealy.model.*
@main def hello: Unit =

  val mach2 = getNDMachine
  // println(mach2.toDot)
  val retVal = mach2.consume("abaabababab")
  println(s"The produced output this run is ${retVal(0)}")
  println(s"The execution trace this run is ${retVal(1)}")
