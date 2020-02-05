class CustomException(message: String) extends Exception(message)

def isPalindrome(list: List[Int]): Boolean = {
  def innerIsPalindrome(list: List[Int], iterationNumber: Int): Boolean = {
    list match {
      case Nil => true
      case _ :: Nil => true
      case _ :: rest if iterationNumber > rest.length => true
      case first :: _ if  first != list(list.length - iterationNumber) => false
      case _ :: rest => innerIsPalindrome(rest, iterationNumber + 1)
    }
  }
  if (list.isEmpty) {
    throw new CustomException("List is empty...")
  }
  innerIsPalindrome(list, 1)
}