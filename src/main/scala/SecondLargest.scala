class CustomException(message: String) extends Exception(message)

class SecondLargest {
  def secondLargestElement(list: List[Int]): Int = {

    def computeSecondLargest(list: List[Int], currentLargest: Int, secondMax: Int): Int = {
      list match {
        case Nil if currentLargest == secondMax => throw new CustomException("All the elements are equal.")
        case Nil => secondMax
        case firstElement :: rest if firstElement > currentLargest => computeSecondLargest(rest, firstElement, currentLargest)
        case firstElement :: rest if firstElement >= secondMax => computeSecondLargest(rest, currentLargest, firstElement)
        case _ :: rest => computeSecondLargest(rest, currentLargest, secondMax)
      }
    }
    if (list.length <= 1)
      throw new CustomException("List does not have enough elements...")
    else
      computeSecondLargest(list, list.head, -1)

  }
}
