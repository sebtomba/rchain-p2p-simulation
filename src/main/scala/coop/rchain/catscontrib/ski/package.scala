package coop.rchain.catscontrib

package object ski {

  /**
    * Returns a function that takes number of arguments and always returns the last argument as a result
    */
  def kp[A, B](x: => B): A => B          = _ => x
  def kp2[A, B, C](x: => C): (A, B) => C = (_, _) => x

}
