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

  @throws(classOf[BadInputException])
  def setInputSequence(argInputSeq: List[Char]): Unit =
    if !argInputSeq.map(inputAlphabet.contains(_)).reduce(_ & _) then
      throw new BadInputException(s"Not in input alphabet: ${argInputSeq}")
    else
      inputSequence = argInputSeq
      pendingInput = true

  def getNextInputToken =
    val token = inputSequence(0)
    inputSequence = inputSequence.slice(1, inputSequence.size)
    if inputSequence.isEmpty then pendingInput = false
    token

  @throws(classOf[NoTransitionFound])  
  def chooseTransition(argTransition: List[Transition]) =
    if argTransition.size == 0 then
      throw new NoTransitionFound(
        s"No transitions found from the current state, State: ${currentState}, argTransition: ${argTransition}"
      )
    else
      val randomIndex = scala.util.Random.nextInt(argTransition.size)
      argTransition(randomIndex)

  def nonDeterministicConsume =
    while pendingInput do
      try
        var possibleTransitions =
          getApplicableTransitions(currentState, getNextInputToken)
        var actualTransition = chooseTransition(possibleTransitions)
        takeTransition(actualTransition)
      catch 
        case ntf: NoTransitionFound => println(ntf.getMessage)
        case tna: TransitionNotApplicable => println(tna.getMessage)

  @throws(classOf[TransitionNotApplicable])
  def takeTransition(argTransition: Transition) =
    if argTransition.transitionSource.getName.equals(currentState.getName) then
      argTransition.setTaken
      processOutput(argTransition.transitionOutput)
      currentState = argTransition.tranditionDest
    else
      throw new TransitionNotApplicable(
        s"Transition not applicable from this state. arg: $argTransition, current transitions source" +
          s"${argTransition.transitionSource}."
      )

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
