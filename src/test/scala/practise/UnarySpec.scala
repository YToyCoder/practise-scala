package prcatise

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import practise.operators.Unary

class UnarySpec extends AnyFlatSpec with Matchers {
  "unary" should "test" in {
    print((2).unary_-)
  }


  "unary" should ("self define unary") in {
    print(new Unary("init").unary_-())
  }

}
