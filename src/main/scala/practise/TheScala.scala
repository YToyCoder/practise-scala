package practise

object TheScala{

  @main
  def main = 
    pattern_bindings
    scala3
    type_member

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
      type SuitableFood <: Food
      def eat(food: SuitableFood): Unit
    end Animal

    class Rice extends Food
    class Cat extends Animal:
      type SuitableFood = Rice
      override def eat(food: SuitableFood): Unit = println(s"eating ${food.getClass().getName()}")
    end Cat
    (new Cat).eat(new Rice)
}