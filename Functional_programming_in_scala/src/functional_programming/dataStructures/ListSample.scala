
//sealed trait
sealed trait List[+A]

case object None extends List[Nothing]
case class Cons[+A](head:A,tail:List[A]) extends List[A]

object List {
  def sum(ints:List[Int]): Int = ints match {
    case None => 0
    case Cons(x,y) => x + sum(y)
  }
  def product(ints:List[Double]): Double = ints match {
    case None => 1
    case Cons(x,y) => x * product(y)
  }
  
  def apply[A](as: A*): List[A] =
    if(as.isEmpty) None
    else Cons(as.head,apply(as.tail : _*))
}


object killer2 {
  def main(args: Array[String]): Unit = {
    val ab = List(2,4,5,9,6)
  	println("total "+List.sum(ab))
  	//Question 1:
  	//What is the result of following:
  	
  	val x = List(1,2,3,4,5) match {
      case Cons(x,Cons(2,Cons(4,_))) => x
      case None => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ =>101
    }
    println(x)
 }
}