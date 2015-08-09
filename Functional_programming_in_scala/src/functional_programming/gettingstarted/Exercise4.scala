package functional_programming.gettingstarted

/**
 * @author shao
 */
//currying, converts a function of N arguments into a function
//of one argument that returns another function as its result
object Exercise4 {
  def curry[A,B,C](f:(A,B)=>C): A=>(B => C) ={
     a => b => f(a,b)
  }
  
  //use case
  def main(args: Array[String]){
    var a = curry((a:Int,b:Int)=>"good")
    print(a(6)(9))
  }
}