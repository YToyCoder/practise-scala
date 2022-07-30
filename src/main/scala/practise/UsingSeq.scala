package practise

object UsingSeq {

  def createSeq() : Seq[String] = {
    // val seq : Seq[String] = 
    Seq("1", "2", "3")
    // seq.mkString
  }

  private def one2ten : Seq[Int] = for(i <- 1 until 10) yield i
  private def ten2twenty : Seq[Int] = for(i <- 10 to 20) yield i
  private def to(from : Int, end : Int) : Seq[Int] = 
    if(from < end) 
      for(i <- from to end) yield i
    else 
      Seq.empty

  def concat : Seq[Int] = {
    one2ten concat ten2twenty
  }

  def diff : Seq[Int] = {
    val one2Twenty = to(1, 20)
    one2Twenty diff one2ten
  }

  def min : Int = {
    val seq = ten2twenty
    seq min ((a : Int , b : Int) => a - b)
    val v = seq minBy (a => (Math.pow(a, 2) - 16 * a)) 
    println(s"v is $v")
    0
  }


}
