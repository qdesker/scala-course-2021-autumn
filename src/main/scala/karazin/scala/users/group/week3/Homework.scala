package karazin.scala.users.group.week3

import scala.annotation.tailrec

object Homework:
  
  // Peano numbers
  abstract class Nat:
    def isZero: Boolean
    def predecessor: Nat
    def successor: Nat = Succ(this)
    
    infix def + (that: Nat): Nat
    
    infix def - (that: Nat): Nat
    
    // Optional task
    def toInt: Int
    
    // Optional task
    def fromInt(int: Int): Nat =
      require(int >= 0, "Not a natural number")
      @tailrec
      def intRec(int: Int, acc: Nat): Nat =
        if int == 0
          then acc
        else intRec(int - 1, acc.successor)

      intRec(int, Zero)
  
    override def toString: String = s"Nat($predecessor)"
  
  type Zero = Zero.type 
  object Zero extends Nat:
    def isZero: Boolean = true
    def predecessor: Nat = throw new Exception("0 doesn't have a predecessor")
    
    infix def +(that: Nat): Nat = that
    
    infix def -(that: Nat): Nat =
      require(that.isZero, "cannot subtract from zero")
      this
    
    // Optional task
    def toInt: Int = 0

    override def toString: String = "Zero"
    override def equals(obj: Any): Boolean = obj match {
      case z: Zero => true
      case _ => false
    }

  class Succ(n: Nat) extends Nat:
    def isZero: Boolean = false
    def predecessor: Nat = n
    
    infix def +(that: Nat): Nat = Succ(n + that)
    
    infix def -(that: Nat): Nat =
      if that.isZero
        then this
      else n - that.predecessor
    
    // Optional task
    def toInt: Int =
      @tailrec
      def intRec(n: Nat, acc: Int): Int =
        if n.isZero
        then acc
        else toIntRec(n.predecessor, acc + 1)
      intRec(n, acc = 1)

    override def equals(obj: Any): Boolean = obj match {
      case zero: Zero => false
      case nat: Nat => this.predecessor ==  nat.predecessor
      case _ => false
    }

