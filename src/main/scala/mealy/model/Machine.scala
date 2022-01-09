package mealy.model
class Machine(
    private val states: List[State],
    private val initialState: State,
    private val inputAlphabet: Set[Char],
    private val outputAlphabet: Set[Char],
    private val machineTransitions: List[Transition]
):
  private var producedOutput: List[Char]     = List.empty
  private var machineTrace: List[Transition] = List.empty
  private var pendingInput: Boolean          = false
  private var inputSequence: List[Char]      = List.empty
  private var currentState: State            = initialState

  for transition <- machineTransitions do
    transition.transitionSource.addOutgoingTransition(transition)
    transition.tranditionDest.addIncominTransition(transition)

  /* Getters for fields */
  def machineStates         = states
  def machineInitialState   = initialState
  def machineInputAlphabet  = inputAlphabet
  def machineOutputAlphabet = outputAlphabet
  def getMachineTransitions = machineTransitions
  def getProducedOutput     = producedOutput
  def getTrace              = machineTrace
  def getPendingInput       = pendingInput
  def getInputSequence      = inputSequence
  def getCurrentState       = currentState

  /* Setters for secondary fields */
  def setProduced(argList: List[Char])     = producedOutput = argList
  def setTrace(argTrace: List[Transition]) = machineTrace = argTrace
  def setCurrent(argState: State)          = currentState = argState

  def setInputSequence(argInputSeq: String) =
    if !argInputSeq.map(inputAlphabet.contains(_)).reduce(_ & _) then
      throw BadInputException(
        s"Input sequence contains element(s) not in input alphabet, argInputSequence: $argInputSeq, inputAlphabet: $inputAlphabet."
      )
    else
      inputSequence = argInputSeq.toList
      pendingInput = true

  def getNextInputToken: Char =
    val token = inputSequence.head
    inputSequence = inputSequence.slice(1, inputSequence.size)
    if inputSequence.isEmpty then pendingInput = false
    token

  def consume =
    machineTrace = List.empty
    producedOutput = List.empty
    while pendingInput do
      try
        val possibleTransitions = getApplicableTransitionsFrom(currentState, getNextInputToken)
        val actualTransition    = chooseTransition(this, possibleTransitions)
        takeTransition(actualTransition)
      catch
        case ex: (NoTransitionFound | TransitionNotApplicable) => println(ex.getMessage)
        case ex: Exception                                     => println(ex.getMessage)

    (producedOutput, machineTrace)

  def consume(argInput: String): Tuple2[List[Char], List[Transition]] =
    try
      setInputSequence(argInput)
      consume
    catch case ex: BadInputException => println(ex.getMessage)

    // Surprising that i need to explicitly mention this tuple
    // i thought the function before the catch would take care of this
    (producedOutput, machineTrace)

  def consumeToken =
    if pendingInput then
      try
        val possibleTransitions = getApplicableTransitionsFrom(currentState, getNextInputToken)
        val actualTransition    = chooseTransition(this, possibleTransitions)
        takeTransition(actualTransition)
      catch
        case ex: (NoTransitionFound | TransitionNotApplicable) => println(ex.getMessage)
        case ex: Exception                                     => println(ex.getMessage)
    else throw NoPendingInput("Cannot consume token: machine not pending.")

  private def takeTransition(argTransition: Transition): Unit =
    argTransition.setTaken
    currentState = argTransition.tranditionDest
    machineTrace = machineTrace :+ argTransition
    processTransition(argTransition)

  private def processTransition(argTransition: Transition): Unit =
    producedOutput = producedOutput :+ argTransition.transitionOutput
    val sourceName = argTransition.transitionSource.getName
    val destName   = argTransition.tranditionDest.getName
    val trigger    = argTransition.transitionTrigger
    val output     = argTransition.transitionOutput
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
        val destName   = transition.tranditionDest.getName
        val trigger    = transition.transitionTrigger
        val output     = transition.transitionOutput
        accumulator += s"$sourceName -> $destName [label=\"$trigger/$output\"];\n"
        accumulator += "      "
      accumulator
    }
    }"""

  override def toString: String =
    s"Machine{states=${states}, initial = ${initialState}, transitions=${machineTransitions}}"
