package functional_programming.dataStructures

/**
 * @author shao
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
  
 
  //Exercise 2, remove the first element from the list.
  def tail[A](as : List[A]): List[A] = as match {
      case Nil => Nil
      case Cons(head,tail) => tail
  } 
  
  //Exercise 3, remove the first N elements from the list.
  def drop[A](n : Int,as : List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(head,tail) => {
      if(n == 1)
        tail
      else 
        drop(n-1,tail)
    }
  }

  //Exercise 4, dropWhile, removes elements from the list prefix
  //as long as they match a predicate
  def dropWhile[A](l:List[A])(f: A => Boolean):List[A] ={
    def helper(l:List[A]):List[A] = l match {
      case Nil => Nil
      case Cons(head,xs) => {
        if(f(head)) 
          helper(xs)
        else
          l
      }
    }
    helper(l)
  }
  
  //Exercise 5, replace the first element of a List with a different value
  def setHead[A](newItem:A,as:List[A]):List[A] = as match{
    case Nil => Cons(newItem,Nil)
    case Cons(head,xs) => Cons(newItem,xs)
  }
  
  //Exercise 6, remove the last element.
  def init[A](l: List[A]): List[A] = l match{
    case Nil => throw new IllegalArgumentException
    case Cons(x,Nil) => Nil
    case Cons(x,as) => Cons(x,init(as))
  }
  
  //foldRight function.!!!!
  def foldRight[A,B](l: List[A], z:B)(f:(A,B) => B):B = l match{
    case Nil => z
    case Cons(k,xs) => f(k,foldRight(xs,z)(f))
  }
  
  def sum2(l: List[Int]) = {
    foldRight(l,0)(_+_)
  }
  
  def product2(l: List[Double]) = { 
    foldRight(l,1.0)(_ * _)
  }
  
  
  def init1[A](l:List[A]):List[A] = l match{
    case Nil => throw new IllegalArgumentException
    case Cons(x,Nil) => Nil
    case Cons(x,ls) => Cons(x,init1(ls))
  }
  
  //Compute the length of a list using foldRight
  def length[A](l: List[A]): Int = {
    foldRight(l,0)((_,acc) => acc+1)
  }
  
  //foldLeft function.!!!!
  def foldLeft[A,B](l: List[A], z:B)(f:(B,A) => B):B = l match{
    case Nil => z
    case Cons(x,xs) => foldLeft(xs,f(z,x))(f)
  }
  
  def sumL(l:List[Double])= {
    foldLeft(l,0.0)(_ + _)
  }
  
  def productL(l:List[Double]) = {
    foldLeft(l,1.0)(_ * _)
  }
}
  

object silly_List{
  def main(args: Array[String]){
    val example = Cons(1,Cons(2,Cons(3,Nil)))
    val example3 = Cons(3,Nil)
    val example2 = List(1)
    print(List.foldLeft(example,0)(_+_))
    
  }
}

