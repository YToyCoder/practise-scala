package practise.adventofcode

import scala.io.Source

object Codes {
  @main
  def the_2022_day1() = {
    val input = Source.fromFile("input.txt")
    val fileStr = input.mkString
    val max = fileStr.split("\n\r")
    .map(_.split("\r").map(_.tail.toInt).reduce((a,b)=> a + b))
    .max
    println(s"the max value ${max}")
    input.close()
  }
}
