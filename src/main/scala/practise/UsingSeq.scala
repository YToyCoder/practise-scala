package practise

object UsingSeq {

  def createSeq() : Seq[String] = {
    // val seq : Seq[String] = 
    Seq("1", "2", "3")
    // seq.mkString
  }

  private def one2ten : Seq[Int] = for(i <- 1 until 10) yield i
  private def ten2twenty : Seq[Int] = for(i <- 10 to 20) yield i

  def concat : Seq[Int] = {
    one2ten concat ten2twenty
  }


}
