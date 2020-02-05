class FirstEvenNumber {
  def firstEven(list: List[Int]): Int = {
    list match {
      case Nil => throw new CustomException("Even number not present.")
      case firstElement :: rest if firstElement == 0 => firstEven(rest)
      case firstElement :: rest if firstElement % 2 == 0 => firstElement
      case _ :: rest => firstEven(rest)
    }
  }
}
