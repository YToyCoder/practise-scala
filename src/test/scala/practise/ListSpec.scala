package practise

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.language.postfixOps

class ListSpec extends AnyFlatSpec with Matchers { 


  "List factory method" should "create a List Object" in {
    val days : List[String] = List("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
    days(0) shouldEqual "Sunday"
    days.length shouldEqual 7
  }

  "Weekends" should "get first day of weekends" in {
    new UsingList().weekends() shouldEqual "Sunday"
  }

  "List prepend" should "faster than append" in {
    val code = new UsingList()
    val pLag = code.prepend() 
    val aLag = code.append()
    aLag > pLag shouldBe true
  }

  "List :::" should " nothing to show" in {
    val code = new UsingList()
    code.prefix()
  }

  "List ++:" should " nothing to show" in {
    val code = new UsingList()
    code ++
  }

  "List case class ::" should "create a List" in {
    val code = new UsingList()
    code.`::op` shouldEqual "-"
  }
}
