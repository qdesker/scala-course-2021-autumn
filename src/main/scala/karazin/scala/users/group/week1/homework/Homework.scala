package karazin.scala.users.group.week1.homework

import scala.annotation.tailrec
/**
 * Preface
 * Implement all the things with ???.
 * All implementations must be tail-recursive is possible.
 * Feel free to use internal methods/functions.
 * Feel free to adjust signatures of hacked methods/functions.
 *
 * 1. Boolean Operators
 * Required task
 * Implement eager boolean operations Not, And and Or.
 * Requirements:
 * a) the function should not use embedded boolean operations
 * b) the functions should be eager
 * c) the function should use `if` expression and `true` and `false` boolean literals 
 *
 * 2. Fermat Numbers
 * Required task
 * Implement function which should calculate n-th Fermat number:
 * Fn = 2 ^ (2 ^ n) + 1, n is non-negative
 * Requirements:
 * a) the function should not use multiplication and power operations
 * b) the functions should only use addition operation
 * For more details @see https://en.wikipedia.org/wiki/Fermat_number
 *
 * 3. Look-and-say Sequence
 * Required task
 * Implement function which should calculate n-th number of Look-and-say sequence
 * For more details @see https://en.wikipedia.org/wiki/Look-and-say_sequence
 *
 * 4. Kolakoski sequence
 * Optional super challenging task
 * Implement function which should calculate n-th number of Kolakoski sequence
 * For more details @see https://en.wikipedia.org/wiki/Kolakoski_sequence
 */

object Homework :

  object `Boolean Operators` :

    def not(b: Boolean): Boolean = if b then false else true

    def and(left: Boolean, right: => Boolean): Boolean = if left then right else false

    def or(left: Boolean, right: => Boolean): Boolean = if left then true else right

  end `Boolean Operators`

  object `Fermat Numbers` :

    val multiplication: (BigInt, BigInt) => BigInt = (first, second) => {

      @tailrec
      def mul(first: BigInt, second: BigInt, acc: BigInt): BigInt =
        if first == 0
        then acc
        else mul(first - 1, second, acc + second)

      mul(first, second, 0)
    }

    val power: (BigInt, BigInt) => BigInt = (first, second) => {

      @tailrec
      def pow(first: BigInt, second: BigInt, acc: BigInt): BigInt =
        if second == 0 then acc
        else pow(first, second - 1, multiplication(acc, first))

      pow(first, second, 1)
    }

    val fermatNumber: Int => BigInt = (n) => power(2, power(2, n)) + 1

  end `Fermat Numbers`

  object `Look-and-say Sequence` :
    val lookAndSaySequenceElement: Int => BigInt = (n: Int) => {
      val regex = """(\d)(\1*)""".r

      @tailrec
      def lookAndSay(n: Int, acc: String): String =
        if n == 0
        then acc
        else lookAndSay(
          n - 1,
          regex.findAllMatchIn(acc).map(m => m.group(0).length.toString +
            m.group(1)).mkString)

      BigInt(lookAndSay(n, "1"))
    }


  end `Look-and-say Sequence`

end Homework