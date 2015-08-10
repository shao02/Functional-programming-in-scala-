
// fibonacci with tail recursion
/*
object fibonacci {
  def fibo(n:Int,pre:Int,prepre:Int):Int={
  	if(n==1) prepre
  	else fibo(n-1,prepre,pre+prepre)
  	pre+prepre
  }
  
  def partiall[A,B,C](a:A,b:B,f:(A,B)=>C):B=>C ={
  	f(a,_)
  }

	def curry[A,B,C](f:(A,B)=>C):A=>(B=>C)={
		def f(a:A)={
			f(a,_)
		}
	}
}
*/


sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail : List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }
  
  def product(ints: List[Int]): Int= ints match {
    case Nil => 0
    case Cons(x,Nil) => x
    case Cons(x,xs) => x * product(xs)
  }
  
  def apply[A](as: A*): List[A] =
    if(as.isEmpty) Nil
    else Cons(as.head,apply(as.tail: _*))
  
  val example = Cons(1,Cons(2,Cons(3,Nil)))
  val example2 = List(1,2,3)
  
  val total = sum(example)
  print(total)
  
}
