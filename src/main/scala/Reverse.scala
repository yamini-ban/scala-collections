class CustomException(message: String) extends Exception(message)

class Reverse {
  def reverseOfList(list: List[Int]): List[Int] = {
    def innerReverseOfList(list: List[Int], resultantList: List[Int]): List[Int] = {
      list match {
        case Nil if resultantList.length == 0 => throw new CustomException("List is empty...")
        case Nil => resultantList
        case firstElement :: rest => innerReverseOfList(rest, firstElement :: resultantList)
      }
    }
    innerReverseOfList(list, List.empty[Int])
  }

}
