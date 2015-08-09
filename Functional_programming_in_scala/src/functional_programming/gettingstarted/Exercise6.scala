package functional_programming.gettingstarted

/**
 * @author shao
 */
//compose two function
object Exercise6 {
  def compose[A,B,C](f:B => C, g: A=> B): A=>C ={
    a => f(g(a))
  }
}