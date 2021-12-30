import mealy.model.*

@main def hello: Unit =
  val init = new State
  val anotherOne = new State
  val trann = new Transition('a', '1', init, anotherOne)
  val secondTran = new Transition('b', '2', anotherOne, init)
  val thirf = new Transition('b', '4', anotherOne, init)
  val forth = new Transition('b', '5', anotherOne, init)

  val mach = new Machine(
    List(init, anotherOne),
    init,
    Set('a', 'b'),
    Set('0', '1'),
    List(trann, secondTran, thirf, forth)
  )

  mach.setInputSequence(List('a', 'b', 'a'))
  mach.nonDeterministicConsume // Seems to work, next up:
  println
