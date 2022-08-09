package  practise

class UsingList {

  def weekends() : String = {
    val days : List[String] = List("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
    val when = "AM" :: "PM" :: Nil
    days match {
      case firstDay :: otherDays => firstDay
      case List() => "empty"
    }
  }

  def prepend() : Long = {

    def ad[T](ls : List[T], el : T) : List[T] = ls.+:(el)
    time{
      listAdd(ad(_, _))
    }

  }

  def append() : Long = {
    def add[T](ls : List[T], el : T) : List[T] = ls.:+(el)
    time{ 
      listAdd(add(_, _))
    }
  }


  def listAdd[T](op : (List[String], String) => List[String]): Unit = {
    var list : List[String] = List()
    for(i <- 0 to 3000)
      list = op(list, s"$i")

    // println(list.mkString(" "))
  }

  def time(run : => Unit): Long = {
    val start : Long = System.currentTimeMillis() 
    run
    val end : Long = System.currentTimeMillis()
    end - start
  }

  def prefix() : Unit = {
    val pf : List[String] = List("A", "B", "C")
    val ls : List[String] = List("l", "i" , "s" ,"t")
    println((ls ::: pf ).mkString)
  }

  def ++(): Unit = {
    val pf : List[String] = List("A", "B", "C")
    val ls : List[String] = List("l", "i" , "s" ,"t")
    println((ls ++: pf ).mkString)
  }

  // 奇怪的写法
  def `::op` (): String = {
     ("" :: ::("-" , Nil)).mkString
  }

  def the_collection() : Unit = {
    (1 to 5).toList.collect((el: Int) => el match {
      case v => s"$v?"
    }).foreach(println _)
  }
 
}
