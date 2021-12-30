package mealy.model

import scala.util.{Try, Success, Failure}

class Machine(
    private val states: List[State],
    private val initialState: State,
    private val inputAlphabet: Set[Char],
    private val outputAlphabet: Set[Char],
    private val machineTransitions: List[Transition]
):
  private var producedOutput: List[Char] = List.empty
  private var machineTrace: List[Transition] = List.empty
  private var pendingInput: Boolean = false
  private var inputSequence: List[Char] = List.empty
  private var currentState: State = initialState

  for transition <- machineTransitions do
    transition.transitionSource.addOutgoingTransition(transition)
    transition.tranditionDest.addIncominTransition(transition)

  /* Getters for fields */
  def machineStates = states
  def machineInitialState = initialState
  def machineInputAlphabet = inputAlphabet
  def machineOutputAlphabet = outputAlphabet
  def getMachineTransitions = machineTransitions
  def getProducedOutput = producedOutput
  def getTrace = machineTrace
  def getPendingInput = pendingInput
  def getInputSequence = inputSequence
  def getCurrentState = currentState

  /* Setters for secondary fields */
  def setProduced(argList: List[Char]) = producedOutput = argList
  def setTrace(argTrace: List[Transition]) = machineTrace = argTrace
  def setCurrent(argState: State) = currentState = argState
  def setInputSequence(argInputSeq: String): Unit =
    if !argInputSeq.map(inputAlphabet.contains(_)).reduce(_ & _) then
      throw new BadInputException(s"Not in input alphabet: ${argInputSeq}")
    else
      inputSequence = argInputSeq.toList
      pendingInput = true

  def getNextInputToken =
    val token = inputSequence(0)
    inputSequence = inputSequence.slice(1, inputSequence.size)
    if inputSequence.isEmpty then pendingInput = false
    token

  def chooseTransition(argTransition: List[Transition]) =
    if argTransition.size == 0 then
      throw new NoTransitionFound(
        s"No transitions found from the current state, ${currentState}, argTransition: ${argTransition}"
      )
    else
      val randomIndex = scala.util.Random.nextInt(argTransition.size)
      argTransition(randomIndex)

  def nonDeterministicConsume =
    while pendingInput do
      try
        val possibleTransitions =
          getApplicableTransitions(currentState, getNextInputToken)
        val actualTransition = chooseTransition(possibleTransitions)
        takeTransition(actualTransition)
      catch
        case ntf: NoTransitionFound => {
          println(ntf.getMessage); System.exit(0)
        }
        case tna: TransitionNotApplicable => {
          println(tna.getMessage); System.exit(0)
        }

  def takeTransition(argTransition: Transition) =
    if argTransition.transitionSource.getName.equals(currentState.getName) then
      argTransition.setTaken
      currentState = argTransition.tranditionDest
      machineTrace = machineTrace :+ argTransition
      processOutput(argTransition.transitionOutput)
    else
      throw new TransitionNotApplicable(
        s"Transition not applicable from this state. arg: $argTransition, current transitions source" +
          s"${argTransition.transitionSource}."
      )

  def processOutput(output: Char) =
    producedOutput = producedOutput :+ output
    println(s"|$output|, current state is ${currentState.getName}. ")

  def toDot =
    s"""digraph {
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

  def copyMachine =
    val newStates = states.map(s => new State(s.getName))
    val newInitState = getStateByName(currentState.getName, newStates)
    val newTransitions =
      machineTransitions.map(t =>
        new Transition(
          t.transitionTrigger,
          t.transitionOutput,
          getStateByName(t.transitionSource.getName, newStates),
          getStateByName(t.tranditionDest.getName, newStates)
        )
      )

    Machine(
      newStates,
      newInitState,
      inputAlphabet,
      outputAlphabet,
      newTransitions
    )

  def exactMachineCopy =
    var retVal = copyMachine
    retVal.setInputSequence(inputSequence.mkString)
    retVal.setProduced(producedOutput)
    retVal.setCurrent(currentState)
    retVal.setTrace(machineTrace)
    retVal