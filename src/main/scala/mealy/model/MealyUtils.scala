package mealy.model

import scala.util.{Try, Success, Failure}

def getApplicableTransitions(
    argState: State,
    triggerChar: Char
): Try[List[Transition]] =
  def filterPredicate(transition: Transition): Boolean =
    transition.isValid && transition.isTriggeredBy(
      triggerChar
    ) && transition.transitionSource.getName == argState.getName
  val retVal = argState.getOutgoing.filter(filterPredicate)
  if retVal.isEmpty then
    val errorMsg =
      s"No transition found from this state: $argState, trigger: $triggerChar."
    Failure(new NoTransitionFound(errorMsg))
  else Success(retVal)

def getStateByName(argStateName: String, stateArray: List[State]): Try[State] =
  val filteredStateArray = stateArray.filter(_.getName == argStateName)
  if filteredStateArray.isEmpty then
    val errorMsg =
      s"State not found: getStateByName: $argStateName, within this array: $stateArray"
    Failure(new StateNotFound(errorMsg))
  else Success(filteredStateArray.head)

def chooseTransition(
    machine: Machine,
    argTransition: List[Transition]
): Try[Transition] =
  if argTransition.isEmpty then
    Failure(
      new NoTransitionFound(
        s"No transitions found from the current state, ${machine.getCurrentState}, argTransition: ${argTransition}"
      )
    )
  else
    val randomIndex = scala.util.Random.nextInt(argTransition.size)
    Success(argTransition(randomIndex))

def getNDMachine: Machine =
  val s1 = new State("s1")
  val s2 = new State("s2")
  val s3 = new State("s3")
  val t1 = new Transition('a', '1', s1, s3)
  val t2 = new Transition('a', '1', s1, s2)
  val t3 = new Transition('b', '1', s1, s3)
  val t4 = new Transition('b', '2', s1, s2)
  val t5 = new Transition('a', '1', s2, s3)
  val t6 = new Transition('a', '2', s2, s1)
  val t7 = new Transition('b', '2', s2, s2)
  val t8 = new Transition('b', '2', s3, s2)
  val t9 = new Transition('b', '1', s3, s1)
  val ta = new Transition('a', '1', s3, s2)

  Machine(
    List(s1, s2, s3),
    s1,
    Set('a', 'b'),
    Set('1', '2'),
    List(t1, t2, t3, t4, t5, t6, t7, t8, t9, ta)
  )

def getDMachine: Machine =
  val d1 = new State("d1")
  val d2 = new State("d2")
  val d3 = new State("d3")
  val d4 = new State("d4")
  val t1 = new Transition('a', '1', d1, d2)
  val t2 = new Transition('b', '2', d1, d4)
  val t3 = new Transition('b', '1', d2, d1)
  val t4 = new Transition('a', '2', d2, d2)
  val t5 = new Transition('a', '1', d3, d2)
  val t6 = new Transition('b', '1', d3, d1)
  val t7 = new Transition('a', '1', d4, d1)
  val t8 = new Transition('b', '2', d4, d3)

  Machine(
    List(d1, d2, d3, d4),
    d1,
    Set('a', 'b'),
    Set('1', '2'),
    List(t1, t2, t3, t4, t5, t6, t7, t8)
  )
