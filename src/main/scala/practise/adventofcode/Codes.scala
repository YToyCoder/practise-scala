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

  def run_with(name: String,code : (content: String) => Unit ) = {
    val source = Source.fromResource(name)
    code(source.mkString)
    source.close()
  }

  @main
  def the_2022_day5() = {
    run_with("day5.txt",content => {
      val spl = content.split("\n\r")
      def if_first_then_rm(c: Char, s:String):String = if(s(0) == c) s.substring(1) else s 
      val a = spl(0).split("\r").map(i => if(i(0) == '\n') i.substring(1) else i)
      val count = (a(a.length -1).length() + 1)/4
      val stk = a.reverse.tail
      var ls  = stk
      .map(el => el.zipWithIndex.filter((_,ind)=> (ind - 1)%4 == 0))
      .flatMap(el => el)
      .groupBy(_._2)
      .toList
      .sortBy(_._1)
      .map(_._2.map(_._1).filter(_ != ' ').reverse.toList)
      .toList
      println(ls)
      val mvs : Array[Array[Int]] = spl(1).substring(1)
      .split('\r')
      .map(if_first_then_rm('\n', _))
      .map(i => i.split(' ').filter(_.forall(_.isDigit)).map(_.toInt))

      for(i <- mvs;
        mv <- 0 until i(0);
        fromi = i(1) - 1;
        toi = i(2) - 1
        ){
        val from = ls(fromi)
        ls = ls.updated(fromi, from.tail)
        ls = ls.updated(toi, from(0) :: ls(toi))
      }
      println(ls.map(el => if(el == Nil) "" else el(0)).mkString)
    })
  }

  def the_2022_day4() : Unit = {
    val content = contentFromResource("day4.txt")
    val include = (a:Array[Int], b : Array[Int]) => a(0) <= b(0) && a(1) >= b(1)
    val pairs = content.split("\r")
    .map(el => if(el(0) == '\n') el.substring(1) else el)
    .map(el => {
      val tasks = el.split(',').map(i => i.split('-').map(_.toInt))
      val one = tasks(0)
      val two = tasks(1)
      include(one, two) | include(two, one)
    })
    .count(_ == true)
    println(pairs)
  }

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
