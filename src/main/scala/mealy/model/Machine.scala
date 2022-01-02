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
  def machineStates: List[State] = states
  def machineInitialState: State = initialState
  def machineInputAlphabet: Set[Char] = inputAlphabet
  def machineOutputAlphabet: Set[Char] = outputAlphabet
  def getMachineTransitions: List[Transition] = machineTransitions
  def getProducedOutput: List[Char] = producedOutput
  def getTrace: List[Transition] = machineTrace
  def getPendingInput: Boolean = pendingInput
  def getInputSequence: List[Char] = inputSequence
  def getCurrentState: State = currentState

  /* Setters for secondary fields */
  def setProduced(argList: List[Char]): Unit = producedOutput = argList
  def setTrace(argTrace: List[Transition]): Unit = machineTrace = argTrace
  def setCurrent(argState: State): Unit = currentState = argState

  def setInputSequence(argInputSeq: String) =
    setInputSequence_(argInputSeq) match
      case Failure(expt) => println(expt)
      case Success(_)    => ()

  def setInputSequence_(argInputSeq: String): Try[Unit] =
    if !argInputSeq.map(inputAlphabet.contains(_)).reduce(_ & _) then
      Failure(new BadInputException(s"Not in input alphabet: ${argInputSeq}"))
    else
      Success({
        inputSequence = argInputSeq.toList
        pendingInput = true
      })

  def getNextInputToken: Try[Char] =
    if inputSequence.isEmpty then
      Failure(new Exception("InputSequence is empty !"))
    else
      val token = inputSequence.head
      inputSequence = inputSequence.slice(1, inputSequence.size)
      if inputSequence.isEmpty then pendingInput = false
      Success(token)

  def nonDeterministicConsume: Unit =
    machineTrace = List.empty
    producedOutput = List.empty
    while pendingInput do
      for
        triggerChar <- getNextInputToken
        possibleTransitions <- getApplicableTransitions(
          currentState,
          triggerChar
        )
        actualTransition <- chooseTransition(this, possibleTransitions)
      yield takeTransition(actualTransition)

  def takeTransition(argTransition: Transition): Unit =
    argTransition.setTaken
    currentState = argTransition.tranditionDest
    machineTrace = machineTrace :+ argTransition
    processTransition(argTransition)

  def processTransition(argTransition: Transition): Unit =
    producedOutput = producedOutput :+ argTransition.transitionOutput
    val sourceName = argTransition.transitionSource.getName
    val destName = argTransition.tranditionDest.getName
    val trigger = argTransition.transitionTrigger
    val output = argTransition.transitionOutput
    println(
      s"From $sourceName to $destName; trigger: $trigger - output : $output."
    )

  def toDot: String =
    s"""digraph {
      node [shape=point] INIT;
      ${currentState.getName} [shape=\"doublecircle\"];
      node [shape=circle];
      rankdir = LR;
      INIT -> ${initialState.getName};
      ${
      var accumulator = ""
      for transition <- machineTransitions do
        val sourceName = transition.transitionSource.getName
        val destName = transition.tranditionDest.getName
        val trigger = transition.transitionTrigger
        val output = transition.transitionOutput
        accumulator += s"$sourceName -> $destName [label=\"$trigger/$output\"];\n"
        accumulator += "      "
      accumulator
    }
    }"""

  override def toString: String =
    s"Machine{states=${states}, initial = ${initialState}, transitions=${machineTransitions}}"

/*def copyMachine =
    val newStates = states.map(s => new State(s.getName))
    val newInitState = getStateByName(currentState.getName, newStates)
    val newTransitions =
      machineTransitions.map(t =>
        for
          sourceState <- getStateByName(t.transitionSource.getName, newStates)
          destState <- getStateByName(t.tranditionDest.getName, newStates)
        yield new Transition(
          t.transitionTrigger,
          t.transitionOutput,
          sourceState,
          destState
        )
      )

    Try {
      Machine(
        newStates,
        newInitState,
        inputAlphabet,
        outputAlphabet,
        newTransitions
      )
    }

  def exactMachineCopy =
    for retVal <- copyMachine
    yield {
      retVal.setInputSequence(inputSequence.mkString)
      retVal.setProduced(producedOutput)
      retVal.setCurrent(currentState)
      retVal.setTrace(machineTrace)
      retVal
    }*/
