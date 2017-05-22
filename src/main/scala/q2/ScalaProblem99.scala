package q2

object ScalaProblem99 {

  def isAdjacentDup(ls: List[Int]) = ls.head == ls.tail.head

  def compressList[A](ls: List[A]) : List[A] = {
    def helper[A](lsIn: List[A], lsOut: List[A]): List[A] = 
    lsIn match {
      case Nil => lsOut
      case h :: t => helper(t.dropWhile(_ == h), h :: lsOut)
    }
      helper(ls.reverse, List[A]())
  }
    def deDups(lsIn: List[Int]) : List[Int] = {

    def helper(inList: List[Int], outList: List[Int]): List[Int] = {
      inList match {
        case Nil =>  outList

        case h :: t if !t.isEmpty =>
          if(isAdjacentDup(inList))
            helper(t, outList)
          else helper(t, h :: outList)

        case h :: t => helper(t, h :: outList)
      }
    }
    helper(lsIn.reverse, List[Int]())
  }
  def dedupFold[A](ls:List[A] ) : List[A] = {
    ls.reverse.foldLeft(List[A]()){
      (z,r) =>
      if (z.isEmpty || z.head != r)
        r :: z
      else z
    }
  }
  def packList[A](ls: List[A]): List[List[A]] = {

    def helper[A](inList: List[A], outList: List[List[A]] ): List[List[A]] = {
      inList match {
        case Nil => outList
        case h :: t if !t.isEmpty && t.head == h => 
          helper(t.dropWhile(_ == h), (h :: t.takeWhile(_ == h)) :: outList )
        case h :: t =>
          helper(t,  List(h) :: outList )
      }
    }
         helper(ls, Nil).reverse
  }

  def packWithScan[A](ls: List[A]) : List[List[A]] = {
    ls match {
      case Nil => Nil
      case h :: t => ls.span( _ == h)._1 :: packWithScan(t.dropWhile(_ == h))
    }
  }

  def packWithScanTail(ls: List[Char]) : List[List[Char]] = {

    def helper(inList: List[Char], outList: List[List[Char]] ): List[List[Char]] = {
      ls match {
        case Nil =>  outList
        case h :: t =>
          helper(t.dropWhile(_ == h), ls.span( _ == h)._1 :: outList)
      }
    }
    helper(ls, Nil)
  }

  def encode0[A](ls: List[A]): List[(Int, A)] = {
    val z = packWithScan( ls )
    z.zip(z.map(_.size)).map(x => ( x._2, x._1.head) )
  }

  def encode[A](ls: List[A]): List[(Int, A)] = {
    packWithScan( ls ) map(x => (x.length, x.head))
  }

  def decode[A](ls: List[(Int, A)]): List[A] = {
     ls flatMap(x => List.fill(x._1)(x._2) )
  }

  def duplicateN[A](n: Int, ls: List[A]) : List[A] =
    ls.flatMap(x => List.fill(n)(x))

  def duplicate[A](n: Int, ls: List[A]) : List[A] =
    duplicateN(n, ls)

  def drop[A](n: Int, ls: List[A]) : List[A] =
    ls.zipWithIndex.filterNot(x => x._2 % (n) == n-1).map(_._1)

  def split[A](n: Int, ls: List[A]) : (List[A], List[A]) = 
    (ls.take(n), ls.drop(n))
  
}
