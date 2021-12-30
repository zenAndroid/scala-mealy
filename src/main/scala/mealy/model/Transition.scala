package mealy.model

class Transition(
    private val transTrigger: Char,
    private val transOutput: Char,
    private val sourceState: State,
    private val destState: State
):
  private var validTransition: Boolean = true;
  private var transitionTaken: Boolean = false;

  def transitionTrigger = transTrigger
  def transitionOutput = transOutput
  def transitionSource = sourceState
  def tranditionDest = destState
  def isValid = validTransition
  def setTaken = transitionTaken = true
  def isTriggeredBy(char: Char) = transTrigger.equals(char)
  override def toString =
    s"Transition{${sourceState.getName} -> ${destState.getName}, trigger: $transTrigger, output: ${transOutput}}"
