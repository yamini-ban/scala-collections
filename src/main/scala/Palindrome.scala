class CustomException(message: String) extends Exception(message)

class Palindrome {
  def isPalindrome(list: List[Int]): Boolean = {
    def innerIsPalindrome(list: List[Int], lastElement: Int): Boolean = {
      list match {
        case first :: Nil => true
        case first :: _ :: last :: Nil if first != last => false
        case _ :: intermediate :: _ :: Nil => innerIsPalindrome(intermediate)
      }
    }
    if (list.isEmpty) {
      throw new CustomException("List is empty...")
    }
    innerIsPalindrome(list, list.head)
  }
}
