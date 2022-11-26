package practise.operators

class Unary(name : String) {
  def unary_-() : Unary = new Unary(s"-$name")

  override def toString = s"Unary($name)"
}
