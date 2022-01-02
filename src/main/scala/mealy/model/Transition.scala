package mealy.model

class Transition(
    private val transTrigger: Char,
    private val transOutput: Char,
    private val sourceState: State,
    private val destState: State
):
  private var validTransition: Boolean = true;
  private var transitionTaken: Boolean = false;

  def transitionTrigger: Char = transTrigger
  def transitionOutput: Char = transOutput
  def transitionSource: State = sourceState
  def tranditionDest: State = destState
  def isValid: Boolean = validTransition
  def setTaken: Unit = transitionTaken = true
  def isTriggeredBy(char: Char): Boolean = transTrigger.equals(char)
  override def toString: String =
    s"Transition{${sourceState.getName} -> ${destState.getName}, trigger: $transTrigger, output: ${transOutput}}"
