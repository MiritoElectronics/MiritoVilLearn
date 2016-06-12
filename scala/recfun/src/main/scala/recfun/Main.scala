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
      
      //def sumPascal(c: Int, r: Int): Int = ??? 
      
      if (c == 0 || (r - c == 0)) 1
      else (pascal(c-1,r-1) + pascal(c,r-1))   
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      val open = '('
      val close = ')'
      
      def findClose(chars: List[Char], level: Int): Boolean = {
        if (chars.isEmpty) false
        else
          if (chars.head == close && level == 0) balance(chars.tail)
          else 
            if (chars.head == close && level > 0) findClose(chars.tail, level -1)
            else
              if (chars.head == open) findClose(chars.tail, level +1)
              else findClose(chars.tail, level)
      }

      if (chars.isEmpty) true
      else 
        if (chars.head == close) false
        else
          if (chars.head == open && chars.tail.isEmpty) false
          else
            if (chars.head != open) balance(chars.tail)
            else findClose(chars.tail,0)
      }
    
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
        
      var acc: Int = 0
      
      def nextNode(nMoney: Int, /*sCoins: List[Int],*/ nCoins: List[Int]):Int = {
       
        if (nCoins.isEmpty) 0
        else if (nMoney - nCoins.head == 0)  {acc += 1; nextNode(nMoney, nCoins.tail)} //Leaf, adds 1
        
        else if (nMoney - nCoins.head < 0) nextNode(nMoney, nCoins.tail) //leaf, adds 0
        
        else if (nMoney - nCoins.head > 0) {nextNode(nMoney - nCoins.head, nCoins);nextNode(nMoney, nCoins.tail) }    //branch, adds 0
        0
      }
            
      nextNode(money, /*coins.sorted.reverse,*/ coins.sorted.reverse)
      acc
    }
  }
