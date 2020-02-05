class RemoveNth {
  def drop(numberToRemove: Int, list: List[Int]): List[Int] = {
    def innerDrop(numberToRemove: Int, list: List[Int], resultantList: List[Int]): List[Int] = {
      list match {
        case Nil => resultantList
        case _ :: rest if numberToRemove == 0 => innerDrop(-1, Nil, resultantList ::: rest)
        case firstElement :: rest => innerDrop(numberToRemove - 1, rest, resultantList ::: List(firstElement))
      }
    }
    if (numberToRemove > list.length || numberToRemove <= 0)
      throw new Exception(s"${numberToRemove} is out of range...")
    innerDrop(numberToRemove - 1, list, List.empty[Int])
  }
}
