package mealy.model

@throws(classOf[NoTransitionFound])
def getApplicableTransitions(argState: State,triggerChar: Char): List[Transition] =
    def filterPredicate(transition: Transition): Boolean =
      transition.isValid && transition.isTriggeredBy(triggerChar)
    val retVal = argState.getOutgoing.filter(filterPredicate)
    if retVal.isEmpty 
      then 
          throw new NoTransitionFound(
            s"No transition found from this state: $argState, trigger: $triggerChar."
          )
    else
      retVal


def stringToList(str: String) = str.toList