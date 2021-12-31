package mealy.model
class State(
    private var stateName: String = "",
    private var inComing: List[Transition] = List.empty,
    private var outGoing: List[Transition] = List.empty
):
  private val currentId = State.getID

  if stateName.equals("") then stateName = "q" + currentId.toString

  def getName = stateName
  def getOutgoing = outGoing
  def getIncoming = inComing
  def addIncominTransition(t: Transition) = inComing = inComing :+ t
  def addOutgoingTransition(t: Transition) = outGoing = outGoing :+ t

  override def toString =
    s"State{name = ${stateName}, inComing = ${inComing}, outGoing = ${outGoing}}"

object State:
  var id: Int = -1
  def getID: Int =
    id = id + 1
    id
