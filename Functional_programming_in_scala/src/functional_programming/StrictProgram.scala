

class StrictProgram {
  def if2[A](con:Boolean, onTrue: => A, onFalse: => A):A ={
    if(con)
      onTrue
    else
      onFalse
  }
}


trait Stream[+A]{
  def uncons: Option[(A,Stream[A])]
  def isEmpty: Boolean = uncons.isEmpty
//  def toList: List[A] = {
//    val cur = uncons.getOrElse(a:Nothing)
//    cur._1::cur._2.toList
//  }
  
  def toList2: List[A] = {
    def go(s:Stream[A], acc:List[A]): List[A] = s match {
      case cons(h,t) => go(t,h::acc)
      case _ => acc
    }
    go(this, List()).reverse
  }
}


object Stream{
  def empty[A]:Stream[A] = new Stream[A] {
    def uncons = None
  }
  
  def cons[A](hd: => A,tl: => Stream[A]):Stream[A]={
    new Stream[A] {
      lazy val uncons = Some((hd,tl))
    }
  }
  
  def apply[A](as: A*):Stream[A] ={
    if(as.isEmpty) empty
    else cons(as.head,apply(as.tail:_*))
  }
}
