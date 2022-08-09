package example

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HelloSpec extends AnyFlatSpec with Matchers {
  // "The Hello object" should "say hello" in {
  //   Hello.greeting shouldEqual "hello"
  // }

  "The operator" should " workd like " in {
    val v = 1 +Operator.plus()
  }

}

object Operator{
  def plus(): Int = 1

  def fun(): Unit = {
    val v = 1 +plus()
  }
}
