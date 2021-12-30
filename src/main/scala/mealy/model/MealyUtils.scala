package mealy.model

def getApplicableTransitions(state: State,triggerChar: Char): List[Transition] =
    def filterPredicate(transition: Transition): Boolean =
      transition.isValid && transition.isTriggeredBy(triggerChar)
    state.getOutgoing.filter(filterPredicate)