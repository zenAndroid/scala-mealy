import mealy.model.*
import scala.util.Failure
import scala.util.Success

@main def hello: Unit =
  val init = new State
  val anotherOne = new State
  val trann = new Transition('a', '1', init, anotherOne)
  val abcde = new Transition('b', '2', anotherOne, init)
  val thirf = new Transition('b', '4', anotherOne, init)
  val forth = new Transition('b', '5', anotherOne, init)

  val mach = new Machine(
    List(init, anotherOne),
    init,
    Set('a', 'b'),
    Set('0', '1'),
    List(trann, abcde, thirf, forth)
  )

  mach.setInputSequence("abbabababa")
  // mach.nonDeterministicConsume

  // println
  val mach2 = getNDMachine
  mach2 setInputSequence "abab"
  // println(mach2.toDot)
  mach2.nonDeterministicConsume
