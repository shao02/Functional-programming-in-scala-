package functional_programming.gettingstarted

/**
 * @author shao
 */
//Implement isSorted with given comparison function
object Excercise2 {
  def isSorted[A] (as: Array[A],gt: (A,A)=>Boolean):Boolean = {
    def helper(pos : Int):Boolean = {
      if(pos > as.length) throw new IllegalArgumentException
      else if(pos == as.length) true
      else if( gt(as(pos),as(pos+1)) ) false
      helper(pos+1)
    }
    helper(0)
  }
}