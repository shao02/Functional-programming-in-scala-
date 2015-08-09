package functional_programming.gettingstarted

/**
 * @author shao
 */
//uncurry, reverses the transformation of curry.
object Exercise5 {
  def uncurry[A,B,C] (f:A => B => C):(A,B)=>C ={
     (a,b) => f(a)(b)
  }

}