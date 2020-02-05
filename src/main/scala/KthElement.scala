class KthElement {

  def kth(list: List[Int], kthIndex: Int): Int = {

    def innerKth(list: List[Int], kthElement: Int): Int = {
      list match {
        case currentElement :: rest if kthElement == 0 => currentElement
        case _ :: rest => innerKth(rest, kthElement - 1)
      }
    }
    if (kthIndex >= list.length || kthIndex < 0) {
        throw new CustomException(s"${kthIndex}th element does not exist.")
    } else
        innerKth(list, kthIndex)
  }

}
