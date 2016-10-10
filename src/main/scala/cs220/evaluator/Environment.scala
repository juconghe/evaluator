package cs220.evaluator

/**
 * A Binding represents a binding of a variable to a value.
 * @param v the variable
 * @param a the value
 */
case class Binding(v: Var, a: Value) {
  override def toString: String = v + " -> " + a
}

/**
 * An Environment holds bindings from variables to values.
 */
abstract class Environment {
  /**
   * lookup returns `Some(b)` for some binding `b` if the
   * variable `v` is found in the environment; None otherwise.
   * @param v the variable to lookup
   * @return `Some(b)` if variable is found; `None` otherwise
   */
  def lookup(v: Var): Option[Binding]

  /**
   * Extends the environment with a new binding v -> a.
   * @param v the variable
   * @param a the value
   * @return the new [[Environment]]
   */
  def extend(v: Var, a: Value): Environment
  /**
   * Returns the list of bindings.
   * @return the list of bindings
   */
  def toList: List[Binding]
}

/**
 * This is a class representing the *initial* environment. The
 * initial environment is empty.
 */
abstract class InitialEnvironment extends Environment {
  // No elements inside initial Environment, return None
  def lookup(v: Var): Option[Binding] = None
/**
 * An extended environment is an environment that is created
 * with a list of bindings and a previous environment.
 * @param bindings the list of bindings
 * @param prev the previous environment
 */
  def extend(v: Var, a: Value): Environment = {
    val tempBinding = List(Binding(v,a))
    val tempEnv = Environment
    new ExtendedEnvironment(tempBinding,tempEnv)
  }
  // No element inside initial Environment, returns Nil for empty list
  def toList: List[Binding] = Nil
  override def toString: String = "{}"
}

/**
 * An extended environment is an environment that is created
 * with a list of bindings and a previous environment.
 * @param bindings the list of bindings
 * @param prev the previous environment
 */
private class ExtendedEnvironment(val bindings: List[Binding],
                                  val prev: Environment)
                                  extends Environment {
/**
 * The extend method is use to put a Var and a Value into the
 * binding form, then return an Environment that with the new
 * binding added
 * @param v the Var value
 * @param a the Value value
 */
  def extend(v: Var, a: Value): Environment = {
    val tempBinding = Binding(v,a)::bindings
    new ExtendedEnvironment(tempBinding,prev)
  }

/**
 * lookup method recursive match given over the given list by calling
 * the helper method inside
 * @param v the Var value
 * @param bs list of bindings
 */
  def lookup(v: Var): Option[Binding] = {
      def lookupHelper(bs: List[Binding], v: Var ) : Option[Binding] ={
        bs match{
          case Nil                  => None
          case b :: bs if(b.v == v) => Some(b)
          case b :: bs              => lookupHelper(bs,v)
          }
      }
      lookupHelper(bindings,v)
  }

  //return a list of binding in chronological order
  def toList: List[Binding] = bindings.reverse
  override def toString: String =
    "{" + toList.mkString(", ") + "}"
  }

/** The initial empty environment */
object Environment extends InitialEnvironment
