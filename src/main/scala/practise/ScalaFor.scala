package practise

import scala.annotation.unchecked.uncheckedVariance

abstract class Afor[+A]{
  def map[B](f: A => B): Afor[B]
  def flatMap[B](f: A => Afor[B]): Afor[B]
  def withFilter[B](p: A => Boolean): Afor[A] 
  def foreach(b: A => Unit): Unit
  def has_next: Boolean
  def head: A
}


import scala.collection.mutable

// abstract class AIterOnce[+A] extends Afor[A] with AIter[A]

abstract class Als[+A] extends Afor[A]{

  def +:[B >: A](elem:B): Als[B] = LsImpl(elem, this)

  override def map[B](f: A => B): Afor[B] = {
    if this == Nls then Nls 
    else 
      var h: LsImpl[B] = LsImpl(f(head), Nls)
      var t: LsImpl[B] = h
      var rest : Als[A] = tail
      while rest.has_next do
        val nx = LsImpl[B](f(rest.head), Nls)
        t.next = nx
        t = nx
      h
  }

  override def flatMap[B](f: A => Afor[B]): Afor[B] = {
    var rest = this
    var h: LsImpl[B] = null
    var t: LsImpl[B] = null
    while rest.has_next do
      val ls = f(rest.head)
      while ls.has_next do
        val nx = LsImpl[B](ls.head, Nls)
        if h == null then h = nx
        else t.next = nx
        t = nx
    if h == null then Nls else h
  }

  def has_next: Boolean = this != Nls

  override def withFilter[B](p: A => Boolean): Afor[A] =
    var rest = this
    var h: LsImpl[A] = null
    var t: LsImpl[A] = null
    while rest.has_next do
      val el = rest.head
      if p(el) then 
        val nx = LsImpl(el, Nls)
        if h == null then 
          h = nx
        else t.next = nx
        t = nx
    if h == null then Nls else h

  override def foreach(b: A => Unit): Unit = {
    var rest = this
    while rest.has_next do
      b(rest.head)
      rest = rest.tail
  }

  def tail: Als[A]

  def head: A
}

final case class LsImpl[+A](val head: A, var next: Als[A @uncheckedVariance]) extends Als[A]:
  override def tail: Als[A] = next
end LsImpl

case object Nls extends Als[Nothing] {
  override def head: Nothing = ???
  override def tail: Nothing = ???
}

@main
def run(args: String*) = {
  val a : Int => Char = _.toChar
  val k = "A-" +: "B-" +: Nls
  for i <- k 
      el <- i
      if el.isLetter
  do
    println(el)
}