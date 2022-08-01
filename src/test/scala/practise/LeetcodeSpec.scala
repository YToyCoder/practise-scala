package  practise

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.language.postfixOps

class LeetcodeSpec extends AnyFlatSpec with Matchers{
  "Leetcode GenerateTheString" should "has odd" in {
    (Leetcode generateTheString 9) shouldEqual ("q".repeat(9))
  }
}
