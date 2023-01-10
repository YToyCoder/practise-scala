package practise.operators

class Unary(name : String) {
  def unary_- : Unary = new Unary(s"-$name")

  override def toString = s"Unary($name)"
}

import scala.collection.mutable

class Update():
  val map : mutable.Map[Int, String] = mutable.Map()
  def update(id: Int, nw: String) = 
    println(s"update: id(${id}) value(${nw})")
end Update

object Runner {

  @main
  def main = 
    println("runner main")
    val u = new Update()
    u(0) = "value"
}