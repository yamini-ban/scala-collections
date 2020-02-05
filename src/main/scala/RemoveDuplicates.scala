class CustomException(message: String) extends Exception(message)

class RemoveDuplicates {

  def compress(list: List[Int]): List[Int] = {
    def innerCompress(list: List[Int], resultantList: List[Int], currentElement: Int): List[Int] = {
      list match {
        case Nil => resultantList ::: List(currentElement)
        case firstElement :: rest if firstElement != currentElement => innerCompress(rest, resultantList ::: List(currentElement), firstElement)
        case _ :: rest => innerCompress(rest, resultantList, currentElement)
      }
    }
    if (list.length == 0) {
      throw new CustomException("List is empty")
    }
    else if(list.length == 1) {
      list
    }
    else {
      innerCompress(list, List.empty[Int], list.head)
    }
  }
}
