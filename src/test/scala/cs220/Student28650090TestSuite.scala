package cs220
import cs220.evaluator._

import org.scalatest.FunSuite

class Student28650090TestSuite extends FunSuite{

  //test 1
  test("Test a complex expression")
  {
    val envs = Environment.extend(Var("a"),Value(5))
                          .extend(Var("b"),Value(6))
                          .extend(Var("c"),Value(22))
    val list = List(Assign(Var("a"),Number(5)),
                    Assign(Var("b"),Number(6)),
                    Assign(Var("c"),Mul(Add(Var("a"),Var("b")),Number(2))),
                    Add(Var("c"),Number(1))
                  )
    val result = Evaluator.eval(Program(list),Environment)
    assert(result.value == Value(23))
    assert(result.env.toString == envs.toString)
  }

  //test 2
  test("Test a empty program expression")
  {
    try {
      val list: List[Expr] = Nil
      Evaluator.evalProgram(Program(list), Environment)
      fail("can't evaluate empty expression!")
    }
    catch {
      case _: RuntimeException      => assert(true)
    }
  }

  //test 3
  test("Test a value that is not in the Environment")
  {
    try {
      val v = Var("a")
      val num = Value(5)
      val env = Environment.extend(v,num)
      Evaluator.eval(Var("b"),env)
      fail("value not exist")
    }
    catch {
      case _: RuntimeException      => assert(true)
    }
  }

  //test 4
  test("Test an funny expression")
  {
    val envs = Environment.extend(Var("a"),Value(5))
                          .extend(Var("b"),Value(6))
                          .extend(Var("c"),Value(22))
                          .extend(Var("d"),Value(8))
    val list = List(Assign(Var("a"),Number(5)),
                    Assign(Var("b"),Number(6)),
                    Assign(Var("c"),Mul(Add(Var("a"),Var("b")),Number(2))),
                    Assign(Var("d"),Number(8))
                    )
    val result = Evaluator.eval(Program(list),Environment)
    assert(result.value == Value(8))
    assert(result.env.toString == envs.toString)
  }

  //test 5
  test("List of expression without add mul div or sub")
  {
    val envs = Environment.extend(Var("a"),Value(5))
                          .extend(Var("b"),Value(6))
                          .extend(Var("c"),Value(5))
                          .extend(Var("d"),Value(5))
    val list = List(Assign(Var("a"),Number(5)),
                    Assign(Var("b"),Number(6)),
                    Assign(Var("c"),Var("a")),
                    Assign(Var("d"),Var("c"))
                   )

    val result = Evaluator.eval(Program(list),Environment)
    assert(result.value == Value(5))
    assert(result.env.toString == envs.toString)
  }

  //test 6
  test("Multiple expression")
  {
    val envs = Environment.extend(Var("a"),Value(5))
                          .extend(Var("b"),Value(6))
                          .extend(Var("c"),Value(1))
                          .extend(Var("d"),Value(8))
    val list = List(Assign(Var("a"),Number(5)),
                    Assign(Var("b"),Number(6)),
                    Assign(Var("c"),Sub(Var("b"),Var("a"))),
                    Assign(Var("d"),Div(Number(8),Var("c"))),
                    Mul(Var("a"),Add(Var("b"),Var("c")))
                    )
    val result = Evaluator.eval(Program(list),Environment)
    assert(result.value == Value(35))
    assert(result.env.toString == envs.toString)
  }

  //test 7
  test("expressions without update environment")
  {
    val envs = Environment

    val list = List(Add(Number(5),Mul(Div(Number(20),Number(4)),
                    Sub(Number(25),Number(20))))
                  )
   val result = Evaluator.eval(Program(list),Environment)
   assert(result.value == Value(30))
   assert(result.env.toString == "{}")
  }

  //test 8
  test("Assign value that already exist")
  {
    try{
      val list = List(Assign(Var("a"),Number(5)),
                      Assign(Var("a"),Number(6))
                     )
      Evaluator.eval(Program(list),Environment)
      fail("Value can't be reassign")
    }
    catch{
      case _: RuntimeException  => assert(true)
    }
  }

  //test 9
  test("test each Simple expression")
  {
    try{
      val evaluator = Evaluator
      val env = Environment
      val r1 = evaluator.evalAdd(Add(Number(1),Number(2)),env)
      assert(r1.value == Value(3))
      val r2 = evaluator.evalSub(Sub(Number(2),Number(1)),env)
      assert(r2.value == Value(1))
      val r3 = evaluator.evalMul(Mul(Number(2),Number(1)),env)
      assert(r3.value == Value(2))
      val r4 = evaluator.evalDiv(Div(Number(2),Number(1)),env)
      assert(r4.value == Value(2))
    }
    catch{
      case _: RuntimeException  =>  fail()
    }
  }

  test("test lookup")
  {
    val envs = Environment.extend(Var("a"),Value(5))
                          .extend(Var("b"),Value(6))
                          .extend(Var("c"),Value(1))
                          .extend(Var("d"),Value(8))
    envs.lookup(Var("c")) match {
      case Some(x)    => assert(true)
      case None       => fail()

    envs.lookup(Var("f")) match {
      case None       => assert(true)
      case Some(x)    => fail()
    }
    }
    }
}
