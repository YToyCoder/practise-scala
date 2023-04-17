package practise

object TheScala{

  def main = 
    pattern_bindings
    scala3
    type_member
    list_op_timing_comp

  def pattern_bindings = 
    // https://docs.scala-lang.org/scala3/reference/changed-features/pattern-bindings.html

    // Bindings in Pattern Definitions 
    val ls: List[String] = "a" :: "b" :: Nil
    val m :: n :: _ = ls: @unchecked 
    println(s" ${m} ${n}")

    // Pattern Bindings in for Expressions
    def pb_for = 
      val elems = List((1,2), "string", (3,4))
      for case (x, y) <- elems 
      yield (x,y)
    println(pb_for) // List((1,2), (3,4))
  
  def scala3 = 
    // https://docs.scala-lang.org/scala3/reference/new-types/intersection-types.html
    println("scala 3 feature")

    // Intersection Types
    def intersection_types = 
      trait Resettable:
        def reset() : Unit
      trait Growable[T]:
        def add(t: T): Unit
      def f(x: Resettable & Growable[String]) = 
        x.reset()
        x.add("hello")
      class RandomT extends Resettable, Growable[String]:
        var s : String = "?"
        override def add(t: String): Unit = s += t
        override def reset(): Unit = s = "" 
        override def toString(): String = s
      end RandomT

      val rt =new RandomT
      println(rt)
      f(rt)
      println(rt)
    intersection_types

  def type_member: Unit = 
    trait Food
    trait Animal :
      type SuitableFood >: BottomFood <: Food 
      def eat(food: SuitableFood): Unit
    end Animal
    class BottomFood extends Rice with Food
    class Rice extends Food
    class Cat extends Animal:
      type SuitableFood = Rice
      override def eat(food: SuitableFood): Unit = println(s"eating ${food.getClass().getName()}")
    end Cat
    (new Cat).eat(new Rice)


    class AnimalWhichEatingNothing extends  Animal {
      type SuitableFood = Rice
      override def eat(food: SuitableFood): Unit = println("eat nothing")
    }

  def list_op_timing_comp: Unit = 
    def printt: Unit = println(System.currentTimeMillis())
    val n = 10000
    var o: List[Int] = Nil
    var t: List[Int] = Nil
    printt
    for (i <- 0 to n){
      o = i +: o
    }
    printt

    printt
    for(i <- 0 to n) {
      t = t :+ i
    }
    printt

  object Enums extends Enumeration{val
    b, a = Value;
  }

  def scala_enumeration = println(s"${Enums.a.id} ${Enums.a.toString()}")

  case class Update(var name: String) {
    println(s">> ${name}")
    def update(v: String) = {
      println(s"update ${name} => ${v}")
      name = v
    }
  }

  def scala_update = 
    val one = Update("hello")
    one() = "print"
}
