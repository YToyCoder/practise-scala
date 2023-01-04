package practise.adventofcode

import scala.io.Source

case class CompareLoop(
  val v: Int,
  val content : List[Char], 
  var pre: CompareLoop = null, 
  var next: CompareLoop = null) {

  def find(c : Char): CompareLoop = {
    if(content.contains(c)) this
    else next.find(c)
  }

  def comp(b : CompareLoop) = {
    if(this == b) 3
    else if(this == b.next) 6
    else 0
  }
}

object Codes {
  def contentFromResource(name : String) : String = Source.fromResource(name).mkString
  @main
  def the_2022_day3() : Unit = {
    println(s"${'A'.toInt} ${'a'.toInt}")
    val content = contentFromResource("day3.txt")
    val level = (c : Char) => 
      if(c >= 'a') c - 'a' + 1
      else c - 'A' + 27
    
    val lines = 
      content.split("\r")
      .map(el => if(el(0) == '\n') el.substring(1) else el)
      .map(el => {
        val (one, two) = el.splitAt(el.length()/2)
        one.filter(c => two.contains(c))
        .distinct
        .map(level)
        .reduce(_ + _)
      })
      .reduce(_ + _)
    println(lines)
  }

  def the_2022_day2() = {
    val input = Source.fromResource("day2.txt")
    val str : String = input.mkString
    // A < Y  Y < C C < A
    // A 岩石 B 纸张 C 剪刀
    // X     Y      Z
    val rock  = CompareLoop(1,'A' :: 'X' :: Nil)
    val paper = CompareLoop(2,'B' :: 'Y' :: Nil, rock)
    val seizer= CompareLoop(3,'C' :: 'Z' :: Nil, paper, rock)
    rock.pre =  seizer
    rock.next = paper
    paper.next = seizer
    val res = str.split('\r')
    .map(el => el.length() match{
      case 3 => el
      case _ => el.substring(1) // rm \r
    })
    .map(el => {
      val oo = rock.find(el(2))
      oo.comp(rock.find(el(0))) + oo.v
    })
    .sum
    println(res)
  }

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
