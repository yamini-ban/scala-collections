class CustomException(message: String) extends Exception(message)

class AddDuplicates {
  def duplicate(list: List[Int]): List[Int] = {
    def innerDuplicate(list: List[Int], resultantList: List[Int]): List[Int] = {
      list match {
        case Nil => resultantList
        case first :: rest => innerDuplicate(rest, resultantList ::: List(first, first))
      }
    }
    if (list.length == 0) {
      throw new CustomException("List is empty...")
    }
    innerDuplicate(list, List.empty[Int])
  }
}
