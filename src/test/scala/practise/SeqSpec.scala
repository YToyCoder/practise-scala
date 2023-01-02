package practise

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.language.postfixOps

class SeqSpec extends AnyFlatSpec with Matchers{
  "Seq" should "create a new Seq" in {
    val seq : Seq[String] = (UsingSeq.createSeq())
    seq.length shouldEqual 3
    seq.mkString shouldEqual "123"
    seq(0) shouldEqual "1"
  }

  "Seq concat" should "concat two Seq" in {
    (UsingSeq concat) shouldEqual (for(i <- 1 to 20) yield i)
  }

  "Seq diff" should " -- " in {
    (UsingSeq diff) shouldEqual (for(i <- 10 to 20) yield i)
  }

  "Seq min" should " get min value" in {
    UsingSeq min
  }

}
