package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    def firstColumn(): Boolean = {
      if ((c == 0) && (r > 0)) true else false
    }

    def onDiagonal(): Boolean = {
      if (c == r) true else false
    }

    if (firstColumn()) 1
    else if (onDiagonal()) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)
  }


  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {


    def ifBalanced(cnt: Int, good: Boolean): Boolean = {
      if (cnt >= 0 && good) true else false
    }

    def parValue(c: Char): Int = {
      if (c == '(') 1
      else if (c == ')') -1
      else 0
    }

    def updateStatus(cnt: Int): Boolean = {
      if (cnt >= 0) true else false
    }

    def balanceIter(c: Char, tail: List[Char], cnt: Int, good: Boolean): Boolean = {
      val aux = cnt + parValue(c)
      if (tail.nonEmpty)
        if (!updateStatus(aux)) false
        else
          balanceIter(tail.head, tail.tail, aux, updateStatus(aux))
      else
        ifBalanced(aux, good)
    }

    if (chars.isEmpty) true else balanceIter(chars.head, chars.tail, 0, true)

  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def changeIter(money: Int, coins: List[Int], size: Int): Int = {
      if (money < 0 || size < 0)
        0
      else if (money == 0)
        1
      else
        changeIter(money, coins, size - 1) + changeIter(money - coins(size), coins, size)
    }
    changeIter(money, coins, coins.length-1)
  }

//  def countChange(money: Int, coins: List[Int]): Int = coins match {
//    case Nil => if(money == 0) 1 else 0
//    case c::rs => (0 to money/c) map (k => countChange(money-k*c,rs)) sum
//  }
}
