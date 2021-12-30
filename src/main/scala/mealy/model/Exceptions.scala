package mealy.model

class BadInputException(msg: String) extends Exception(msg) {}
class NoLastChange(msg: String) extends Exception(msg) {}
class NoPendingInput(msg: String) extends Exception(msg) {}
class NoTransitionFound(msg: String) extends Exception(msg) {}
class StateNotFound(msg: String) extends Exception(msg) {}
class TransitionNotApplicable(msg: String) extends Exception(msg) {}
class TransitionNotFound(msg: String) extends Exception(msg) {}