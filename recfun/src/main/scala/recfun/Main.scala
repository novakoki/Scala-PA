package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 100) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (c < 0 || r < 0) throw new IllegalArgumentException
      if (c == 0) 1
      else
        if (c == r) 1
        else pascal(c-1, r-1) + pascal(c, r-1)
    }


  // To check balanced parentheses
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def helper(chars: List[Char], acc: Int): Int = {
        if (chars.isEmpty) acc
        else
          if (acc < 0) -1
          else
            if (chars.head == '(') helper(chars.tail, acc+1)
            else
              if (chars.head == ')') helper(chars.tail, acc-1)
              else helper(chars.tail, acc)
      }
      if (helper(chars, 0) == 0) true else false
    }



  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money < 0) 0
      else
        if (money == 0) 1
        else
          if (coins.isEmpty) 0
          else countChange(money-coins.head, coins) + countChange(money, coins.tail)
    }
  }
