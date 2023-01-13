package practise

trait STrait {
  def fun() = println("strait")
}

class STKlass extends STrait {
  override def fun() = 
    super.fun()
    println("hello")
}

object ScalaTraits {
  def main = 
    println("scala trait")
    val a = new STKlass
    a.fun()
}