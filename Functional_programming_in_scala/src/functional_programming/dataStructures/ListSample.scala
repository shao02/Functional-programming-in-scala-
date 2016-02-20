
//sealed trait
sealed trait List[+A]
sealed trait Tree[+A]

case object Nil extends Tree[Nothing]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left:Tree[A],right: Tree[A]) extends Tree[A]

case object None extends List[Nothing]
case class Cons[+A](head:A,tail:List[A]) extends List[A]{
   override def toString() = {
     head.toString()+","+tail.toString()
   }
}


object Tree {
  def size[A](tr:Tree[A]):Int = tr match {
    case Nil => 0
    case Leaf(a) => 1
    case Branch(l,r) => size(l) + size(r)
  }
  
  def maxTree(tr:Tree[Int]):Int = tr match {
    case Nil => -1
    case Leaf(a) => a
    case Branch(l,r) => maxTree(l) max maxTree(r)
  }
  
  def depth[A](tr:Tree[A]):Int = tr match {
    case Nil => 0
    case Branch(x,y) => 1 + (depth(x) max depth(y))
  }
}


object List {
  def sum(ints:List[Int]): Int = ints match {
    case None => 0
    case Cons(x,y) => x + sum(y)
  }
  def product(ints:List[Double]): Double = ints match {
    case None => 1
    case Cons(x,y) => x * product(y)
  }
  def removeFist[A](ints:List[A]):List[A]= ints match{
    case None => None
    case Cons(x,y) => y
  }
  
  def drop[A](ints:List[A],count:Int):List[A] = ints match {
    case None => None
    case Cons(x,y) => if(count > 0) drop(y,count-1) else ints
  }
  
  def dropIfTrue[A](ints:List[A],a:A=>Boolean):List[A] = ints match {
    case Cons(h,t) if a(h) => dropIfTrue(t,a)
    case _ => ints
  }
  
  def setHead[A](ints:List[A],newhead:A)=ints match{
    case None => List(newhead)
    case Cons(h,t) => append(List(newhead),t)
  }
  
  def append[A](first:List[A],second:List[A]):List[A] = first match {
    case None => second
    case Cons(h,t) => Cons(h,append(t,second))
  }
  
  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case None => z
    case Cons(a,b) => foldRight(b,f(a,z))(f)
  }
  
  def apply[A](as: A*): List[A] =
    if(as.isEmpty) None
    else Cons(as.head,apply(as.tail : _*))
}


object killer2 {
  def main(args: Array[String]): Unit = {
    val ab = List(2,4,5,9,4)
    val ab1 = List(2)
    val none = List()
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
    println("ab:"+ ab)
    println("ab1:"+ ab1)
    println("Remove head ab:"+List.removeFist(ab))
    println("Remove head ab1:"+List.removeFist(ab1))
    println("Remove head none:"+List.removeFist(none))
    
    
    println("Drop from list none:"+List.drop(none,1))
    println("Drop from list ab:"+List.drop(ab,3))
    
    println("Drop if list none:"+List.dropIfTrue(none,(a:Int)=>a>1))
    println("Drop if list ab:"+List.dropIfTrue(ab,(a:Int)=>a<5))
    
    println("Set head ab1:"+List.setHead(ab1, 3))
    println("Set head ab:"+List.setHead(ab, 3))
    
    //println("Test foldRight ab1:"+List.foldRight(ab1, 3)((a:Int,b:Int)=>a+b))
    println("Test foldRight ab1:"+List.foldRight(ab1, 3)(_+_))
    println("Test foldRight ab1:"+List.foldRight(ab1, 3)(_*_))
 }
}