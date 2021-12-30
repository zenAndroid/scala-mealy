package mealy.model

class Machine(
    private var states: List[State],
    private val initialState: State,
    private val inputAlphabet: Set[Char],
    private val outputAlphabet: Set[Char],
    private var machineTransitions: List[Transition]
) {
  var producedOutput: List[Char] = List.empty
  var pendingInput: Boolean = false
  var inputSequence: List[Char] = List.empty
  var currentState: State = initialState

  for transition <- machineTransitions do
    transition.transitionSource.addOutgoingTransition(transition)
    transition.tranditionDest.addIncominTransition(transition)

  def setInputSequence(argInputSeq: List[Char]): Unit =
    if !argInputSeq.map(inputAlphabet.contains(_)).reduce(_ & _) then
      throw new Exception(s"Not in input alphabet: ${argInputSeq}")
    else
      inputSequence = argInputSeq
      pendingInput = true

  /* def setMachineTransitions(argTransitions: List[Transition]): Unit =
    for transition <- argTransitions do
      machineTransitions = machineTransitions.appended(transition)
      transition.sourceState.addOutgoingTransition(transition)
      transition.destState.addIncominTransition(transition)

  def setMachineTransitions(argTransitions: Transition*): Unit =
    setMachineTransitions(argTransitions.toList) */

  def getNextInputToken =
    val token = inputSequence(0)
    inputSequence = inputSequence.slice(1, inputSequence.size)
    if inputSequence.isEmpty then pendingInput = false
    token

  def chooseTransition(argTransition: List[Transition]) =
    val randomIndex = scala.util.Random.nextInt(argTransition.size)
    argTransition(randomIndex)

  def nonDeterministicConsume =
    while pendingInput do
      var possibleTransitions =
        getApplicableTransitions(currentState, getNextInputToken)
      var actualTransition = chooseTransition(possibleTransitions)
      takeTransition(actualTransition)

  def takeTransition(argTransition: Transition) =
    argTransition.setTaken
    processOutput(argTransition.transitionOutput)
    currentState = argTransition.tranditionDest

  def processOutput(output: Char) = print(output)

  def toDot =
    s"""digraph Automaton {
      node [shape=point] INIT;
      ${currentState.getName} [shape=\"doublecircle\"];
      node [shape=circle];
      rankdir = LR;
      INIT -> ${initialState.getName};
      ${
      var accumulator = ""
      for transition <- machineTransitions do
        accumulator += s"${transition.transitionSource.getName} -> ${transition.tranditionDest.getName} [label=\"${transition.transitionTrigger}/${transition.transitionOutput}\"];\n"
        accumulator += "      "
      accumulator
    }
    }"""

  override def toString =
    s"Machine{states=${states}, initial = ${initialState}, transitions=${machineTransitions}}"

}
