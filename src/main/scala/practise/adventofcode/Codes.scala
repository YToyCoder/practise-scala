package practise.adventofcode

import scala.io.Source
import scala.collection.mutable.ListBuffer

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

import scala.collection.mutable

object LsAndMap{
  def max_ls_size() : Int = 4
}
case class LsAndMap[E](
  var len : Int = 0,
  val counter : mutable.Map[E,Int] = mutable.Map[E,Int](), 
  val ls : mutable.ListBuffer[E] = mutable.ListBuffer[E]()
){
  def +(elem : E):Option[E] = {
    add(elem)
    if(len <= LsAndMap.max_ls_size()){
      Option.empty
    }else Option(remove(elem))
  }

  private[this] def add(elem : E) = {
    ls.addOne(elem)
    counter.addOne((elem, counter.getOrElse(elem, 0) + 1)) 
    len += 1
  }

  private[this] def remove(elem : E): E = {
    // val ret = ls.reverse.last
    val ret = ls.remove(0)
    counter.updateWith(ret) {count => Option(( count.getOrElse(1) - 1)) }
    len -= 1
    ret
  }

  def all_unique : Boolean = len == LsAndMap.max_ls_size() && counter.forall((_, count) => count <= 1)
}

abstract class FSEl {
  def name: String
  def isfile: Boolean = false
  def isdir: Boolean = false
  def subs: mutable.ListBuffer[FSEl]
}
case class FSFile(val name: String, val size: Int) extends FSEl {
  def subs: mutable.ListBuffer[FSEl] = mutable.ListBuffer.empty
  override def isfile = true
}

case class FSDir(val name: String, val up: FSDir, val subs : mutable.ListBuffer[FSEl] = ListBuffer()) extends FSEl {
  def add(el : FSEl) = subs.addOne(el)
  def find(sub: String) : FSDir = subs.find(_.name == sub).get.asInstanceOf[FSDir]
}

object Codes {
  def contentFromResource(name : String) : String = Source.fromResource(name).mkString

  def run_with(name: String,code : (content: String) => Unit ) = {
    val source = Source.fromResource(name)
    code(source.mkString)
    source.close()
  }

  def split_to_lines(name: String): Iterator[String] = Source.fromResource(name).getLines()

  def the_2022_day7() = {
    val lines = split_to_lines("day7.txt")
    val is_cmd = (s : String) => s(0) == '$'
    val root : FSDir = FSDir("/", null)
    var cur : FSDir = null
    for(line <- lines){
      val els = line.split(' ')
      if(is_cmd(line)){
        els(1) match {
          case "cd" => els(2) match {
              case "/" => cur = root
              case ".." => cur = cur.up
              case _ => cur = cur.find(els(2))
            }
          case "ls" => {/** do nothing */}
        }
      }else{
        cur.add(
          if(els(0) == "dir"){
            FSDir(els(1), cur)
          }else {
            FSFile(els(1), els(0).toInt)
          }
        )
      }
    }
    var ret : Int = 0
    def get_total(path : FSDir): Int = {
      val all_in = (for sub <- path.subs
        v = sub match {
          case a:FSDir => { get_total(a) }
          case b:FSFile => { b.size }
          case _ => 0
        }
      yield v)
      .reduce(_ + _)
      if(all_in <= 100000)
        ret += all_in
      all_in
    }
    println(lines.mkString)
    println(get_total(root))
    println(s"res: ${ret}")
  }

  def the_2022_day6() = {
    
    def test_for(c_str : String): Int = {
      val counter: LsAndMap[Char] = LsAndMap()
      def fist_marker(loc: Int): Int = {
        if(counter.all_unique) loc
        else if(loc >= c_str.length()) -1
        else {
          counter.+(c_str(loc))
          fist_marker(loc + 1)
        } 
      }
      fist_marker(0)
    }
    println(test_for("mjqjpqmgbljsphdztnvjfqwrcgsmlb"))
    println(test_for("bvwbjplbgvbhsrlpgdmjqwftvncz"))
    println(test_for("nppdvjthqldpwncqszvftbrmjlhg"))
    println(test_for("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"))
    run_with("day6.txt",c => println(test_for(c)))
  }

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

  @main
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
    val lines = str.split('\r')
    .map(el => el.length() match{
      case 3 => el
      case _ => el.substring(1) // rm \r
    })

    val part_one = lines
    .map(el => {
      val oo = rock.find(el(2))
      oo.comp(rock.find(el(0))) + oo.v
    })
    .sum

    val part_two = lines
    .map(el => {
      el(2) match {
        case 'X' => rock.find(el(0)).pre.v
        case 'Y' => rock.find(el(0)).v + 3
        case  _  => rock.find(el(0)).next.v + 6
      }
    })
    .reduce(_ + _)
    println(s"part one ${part_one}")
    println(s"part two ${part_two}")
  }

  def the_2022_day1() = {
    val input = Source.fromFile("input.txt")
    val fileStr = input.mkString
    val each_count = fileStr.split("\n\r")
    .map(_.split("\r").map(_.tail.toInt).reduce((a,b)=> a + b))
    println(s"the max value ${each_count.max}")
    val c = each_count.sortInPlace().reverse.splitAt(3)(0).reduce( _ + _ )
    println(c)
    input.close()
  }
}
