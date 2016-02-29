
case class Employee(name:String,department:String)
object ExceptionHandlerV2 {
  def main(args: Array[String]): Unit = {
    val employeeByName:Map[String,Employee] =
    List(Employee("Alice","M&A"),Employee("Bob","CS")).map(e=>(e.name,e)).toMap
    println(employeeByName)
    
    val depart:String = employeeByName.get("Bob").filter(_.department!="M&A").map(_.name).getOrElse("default department")
    println(depart)
  }
  
  def mean(xs:Seq[Double]): Option[Double] = {
    if(xs.length == 0) None
    else Some(xs.sum / xs.length)
  }
  
  def bothMatch_2(pat1: String, pat2: String, s: String): Option[Boolean] = {
    None
  }
  
  // keep that in mind that, flatMap takes a function that converse type A to Option[C]
  // map takes a function that converse type B to 
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    // fun that converse a to c.
    a flatMap( aa => b map (bb => f(aa,bb)))
    None
  }
  
  import java.util.regex._
  def pattern(s:String):Option[Pattern] = {
    try{
      Some(Pattern.compile(s))
    }catch{
      case e: PatternSyntaxException => None
    } 
  }
  
  def mkMatcher_1(pat: String): Option[String => Boolean] ={
    for {
      k <- pattern(pat)
    } yield ((s:String) => k.matcher(s).matches())
  }
  
  def doesMatch(pat: String, s: String): Option[Boolean] ={
    for {
      y <- mkMatcher_1(pat)
    } yield y(s)
  }
  
  def mkMatcher(pat: String): Option[String => Boolean] ={
    pattern(pat) map (p => (s:String) => p.matcher(s).matches())
  }
  
  ///!!!! return a function expects to take Option[A] and return Option[B]... _ is Option[A]
  def lift[A,B](f:A=>B):Option[A] => Option[B] = _ map f
  
  //flatMap[B](f:A=> Option[B]):Option[B]
  
  def variance(xs:Seq[Double]) : Option[Double] = {
    mean(xs) flatMap (mn => mean(xs.map(x => math.pow(x - mn,2))))
  }
  
  
  
  sealed trait Either[+E,+A]
  case class Left[+E](value:E) extends Either[E,Nothing]
  case class Right[+A](value:A) extends Either[Nothing,A]
}