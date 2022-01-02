package mealy.model
class State(
    private var stateName: String = "",
    private var inComing: List[Transition] = List.empty,
    private var outGoing: List[Transition] = List.empty
):
  private val currentId: Int = State.getID

  if stateName.equals("") then stateName = "q" + currentId.toString

  def getName: String = stateName
  def getOutgoing: List[Transition] = outGoing
  def getIncoming: List[Transition] = inComing
  def addIncominTransition(t: Transition): Unit = inComing = inComing :+ t
  def addOutgoingTransition(t: Transition): Unit = outGoing = outGoing :+ t

  override def toString: String =
    s"State{name = ${stateName}, inComing = ${inComing}, outGoing = ${outGoing}}"

object State:
  var id: Int = -1
  def getID: Int =
    id = id + 1
    id
