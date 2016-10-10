package cs220.evaluator

/**
 * An EvaluationException represents a problem with an expression program.
 * An EvaluationException is thrown when there is a problem with
 * evaluating an expression program.
 */
class EvaluationException(msg: String) extends RuntimeException(msg)

/**
 * An EvaluationResult represents the result of an evaluation.
 */
case class EvaluationResult(value: Value, env: Environment)

/**
 * An AbstractEvaluator defines the operations that our evaluator
 * will use to evaluation [[Expr]] objects in an [[Environment]].
 */
abstract class AbstractEvaluator {
  /**
   * eval returns an [[EvaluationResult]] given an expression `expr` and
   * an environment `env`. It determines which of the other `eval` methods
   * to invoke based off of the type of [[Expr]].
   */
  def eval(expr: Expr, env: Environment): EvaluationResult

  /**
   * evalNumber evaluates a [[Number]] `num` in an [[Environment]] `env`.
   */
  def evalNumber(num: Number, env: Environment): EvaluationResult

  /**
   * evalVar evaluates a [[Var]] `v` in an [[Environment]] `env`.
   */
  def evalVar(v: Var, env: Environment): EvaluationResult

  /**
   * evalAdd evaluates an add expression in an [[Environment]] `env`.
   */
  def evalAdd(op: Add, env: Environment): EvaluationResult

  /**
   * evalSub evaluates an subtract expression in an [[Environment]] `env`.
   */
  def evalSub(op: Sub, env: Environment): EvaluationResult

  /**
   * evalMul evaluates a multiply expression in an [[Environment]] `env`.
   */
  def evalMul(op: Mul, env: Environment): EvaluationResult

  /**
   * evalDiv evaluates a divide expression in an [[Environment]] `env`.
   */
  def evalDiv(op: Div, env: Environment): EvaluationResult

  /**
   * evalAssign evaluates an assignment expression in an [[Environment]] `env`.
   */
  def evalAssign(op: Assign, env: Environment): EvaluationResult

  /**
   * evalProgram evaluates a program expression in an [[Environment]] `env`.
   */
  def evalProgram(prog: Program, env: Environment): EvaluationResult
}

// TODO: Part 5 - implement a simple evaluator.
class SimpleEvaluator extends AbstractEvaluator {

/**
 * method eval recursivly call other sub methods in side this class
 * until it reach the tail recursion, whic his a Number or variable
 * @param expr a expression
 * @param env an environment
 */
  def eval(expr: Expr, env: Environment): EvaluationResult = {
      expr match{
        case Number(x)   => evalNumber(Number(x),env)
        case Var(x)      => evalVar(Var(x),env)
        case Add(l,r)    => evalAdd(Add(l,r),env)
        case Sub(l,r)    => evalSub(Sub(l,r),env)
        case Mul(l,r)    => evalMul(Mul(l,r),env)
        case Div(l,r)    => evalDiv(Div(l,r),env)
        case Assign(l,r) => evalAssign(Assign(l,r),env)
        case Program(x)  => evalProgram(Program(x),env)
        case _           => throw new EvaluationException("not a proper expr")
      }
  }

/**convert a number into a value and return an evaluation result
 * with the value and give environment
 * @param num a number
 * @param env an environment
 */
  def evalNumber(num: Number, env: Environment): EvaluationResult =
    new EvaluationResult(Value(num.value),env)

/**
 *look up a variable inside give environment by using the lookup mehtod
 *that is defined in environment class. If the vaule exist, return it
 * with given environemt, otherwise throw exception
 *@param v variable
 *@param env environment
 */
  def evalVar(v: Var, env: Environment): EvaluationResult ={
    env.lookup(v) match{
      case Some(c)   => new EvaluationResult(c.a,env)
      case None      => throw new EvaluationException("Value not exist")
    }
  }

/** call the eval method of the left of given expression then the right.
 * after all of those is done, sum them up.
 * @param op an Add expression
 */
  def evalAdd(op: Add, env: Environment): EvaluationResult =
    new EvaluationResult(Value(eval(op.left,env).value.i
        + eval(op.right,env).value.i),env)

/** call the eval method of the left of given expression then the right.
 * after all of those is done, substract the right from the left.
 * @param op an sub expression
 */
  def evalSub(op: Sub, env: Environment): EvaluationResult =
    new EvaluationResult(Value(eval(op.left,env).value.i
        - eval(op.right,env).value.i),env)

/** call the eval method of the left of given expression then the right.
 * after all of those is done, take the product of them.
 * @param op an Mul expression
 */
  def evalMul(op: Mul, env: Environment): EvaluationResult =
    new EvaluationResult(Value(eval(op.left,env).value.i
        * eval(op.right,env).value.i),env)

/** call the eval method of the left of given expression then the right.
 * after all of those is done, divide the left by the right.
 * @param op an Div expression
 */
  def evalDiv(op: Div, env: Environment): EvaluationResult =
    new EvaluationResult(Value(eval(op.left,env).value.i
        / eval(op.right,env).value.i),env)

/** check if that variable already exist in the environment, if not
 *  call the eval method with the right of given expression
 *  then extend the environemt with the left and the value from the eval
 * @param op an Assign expression
 */
  def evalAssign(op: Assign, env: Environment): EvaluationResult = {
    val leftValue =
      env.lookup(op.left) match{
        case Some(x)  =>  throw new EvaluationException("Can't reassign value")
        case None     =>  op.left
    }
    val rightValue = eval(op.right,env).value
    val ne = env.extend(leftValue,rightValue)
    new EvaluationResult(rightValue,ne)
  }

  // iterate throught the list of expression then return the result of the
  // last expression and final environment
  def evalProgram(prog: Program, env: Environment): EvaluationResult ={
    if(prog.exprs.isEmpty)
    {
      throw new EvaluationException("Program list is empty")
    }
      else{
        var ne = env
        var temp=eval(prog.exprs.head,ne)
        for(expr <- prog.exprs){
          temp = eval(expr,ne)
          ne = temp.env
          }
          new EvaluationResult(temp.value,ne)
          }
          }
        }

/** A factory object for an evaluator. */
object Evaluator extends SimpleEvaluator
