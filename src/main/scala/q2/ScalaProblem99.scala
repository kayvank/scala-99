package q2

object ScalaProblem99 {

  def isAdjacentDup(ls: List[Int]) = ls.head == ls.tail.head

  def compressList[A](ls: List[A]): List[A] = {
    def helper(lsIn: List[A], lsOut: List[A]): List[A] =
      lsIn match {
        case Nil    => lsOut
        case h :: t => helper(t.dropWhile(_ == h), h :: lsOut)
      }
    helper(ls.reverse, List[A]())
  }
  def deDups(lsIn: List[Int]): List[Int] = {
    def helper(inList: List[Int], outList: List[Int]): List[Int] =
      inList match {
        case Nil => outList

        case h :: t if !t.isEmpty =>
          if (isAdjacentDup(inList))
            helper(t, outList)
          else helper(t, h :: outList)

        case h :: t => helper(t, h :: outList)
      }
    helper(lsIn.reverse, List[Int]())
  }

  def dedupFold[A](ls: List[A]): List[A] =
    ls.reverse.foldLeft(List[A]()) { (z, r) =>
      if (z.isEmpty || z.head != r)
        r :: z
      else z
    }

  def packList[A](ls: List[A]): List[List[A]] = {
    def helper(inList: List[A], outList: List[List[A]]): List[List[A]] =
      inList match {
        case Nil => outList
        case h :: t if !t.isEmpty && t.head == h =>
          helper(t.dropWhile(_ == h), (h :: t.takeWhile(_ == h)) :: outList)
        case h :: t =>
          helper(t, List(h) :: outList)
      }
    helper(ls, Nil).reverse
  }

  def packWithScan[A](ls: List[A]): List[List[A]] = ls match {
    case Nil    => Nil
    case h :: t => ls.span(_ == h)._1 :: packWithScan(t.dropWhile(_ == h))
  }

  def packWithScanTail(ls: List[Char]): List[List[Char]] = {
    def helper(
        inList: List[Char],
        outList: List[List[Char]]
    ): List[List[Char]] =
      inList match {
        case Nil => outList
        case h :: t =>
          helper(t.dropWhile(_ == h), ls.span(_ == h)._1 :: outList)
      }

    helper(ls, Nil)
  }

  def encode0[A](ls: List[A]): List[(Int, A)] = {
    val z = packWithScan(ls)
    z.zip(z.map(_.size)).map(x => (x._2, x._1.head))
  }

  def encode[A](ls: List[A]): List[(Int, A)] =
    packWithScan(ls) map (x => (x.length, x.head))

  // P13
  def decode[A](ls: List[(Int, A)]): List[A] =
    ls flatMap (x => List.fill(x._1)(x._2))

  // P14
  def duplicateN[A](n: Int, ls: List[A]): List[A] =
    ls.flatMap(x => List.fill(n)(x))

  // P15
  def duplicate[A](n: Int, ls: List[A]): List[A] =
    duplicateN(n, ls)

  // P16
  def drop[A](n: Int, ls: List[A]): List[A] =
    ls.zipWithIndex.filterNot(x => x._2 % (n) == n - 1).map(_._1)

  // P17
  def split[A](n: Int, ls: List[A]): (List[A], List[A]) =
    (ls.take(n), ls.drop(n))

  // P18
  def slice[A](s0: Int, s1: Int, ls: List[A]): List[A] =
    ls.drop(s0).take(s1 - s0)

  // P19
  def rotate[A](n: Int, ls: List[A]): List[A] =
    if (n > 0)
      ls.drop(n) ::: ls.take(n)
    else if (n < 0) {
      val lsr = ls.reverse
      (lsr.drop(Math.abs(n)) ::: lsr.take(Math.abs(n))).reverse
    } else ls

  // P20
  def removeAt[A](n: Int, ls: List[A]): (List[A], A) =
    (ls.take(n) ::: ls.drop(n + 1), ls.drop(n).head)

  // P21
  def insertAt[A](e: A, n: Int, ls: List[A]): List[A] = {
    lazy val tmp = split(n, ls)
    tmp._1 ::: List(e) ::: tmp._2
  }

  // P22
  def range(s0: Int, s1: Int): List[Int] =
    (s0 to s1).toList

  // P23
  def randomSelect(n: Int, ls: List[Symbol]): List[Symbol] = {
    val r = new java.util.Random
    def helper(
        ndx: Int,
        lsIn: List[Symbol],
        lsOut: List[Symbol]
    ): List[Symbol] =
      if (ndx == 0)
        lsOut
      else {
        val p = removeAt(r.nextInt(lsIn.length), ls)
        helper(ndx - 1, p._1, p._2 :: lsOut)
      }
    helper(n, ls, List[Symbol]())
  }

  def flatMapSublist[A, B](ls: List[A])(f: (List[A]) => List[B]): List[B] =
    ls match {
      case Nil                   => Nil
      case sublist @ (_ :: tail) => f(sublist) ::: flatMapSublist(tail)(f)
    }

  // P31 recursive
  def isEven(n: Int) = n % 2 == 0
  def isOdd(n: Int) = !isEven(n)

  implicit class PrimeFinder(n: Int) {
    def isPrime: Boolean = {
      def helper(x: Int): Boolean =
        if (x <= 1) true
        //else if( isEven(x) ) helper(x-1)
        else if ((n % x) == 0) false
        else helper(x - 1)
      helper(Math.sqrt(n.toDouble).toInt)
    }
  }

  // P31 Functional
  implicit class StreamingPrimeFinder(n: Int) {
    def isPrime2: Boolean =
      (2 to Math.sqrt(n.toDouble).toInt).reverse.toStream
      // .filter(isOdd)
        .find(n % _ == 0)
        .isEmpty
  }

  implicit class PrimeNumbers(maxNum: Int) {
    def primes: Stream[Int] =
      (2 to maxNum).toStream.filter(_.isPrime2)
  }

  implicit class PrimeFactors(n: Int) {

    def primeFactors: List[Int] = {

      def helper(n: Int, primes: Stream[Int], res: List[Int]): List[Int] =
        if (n <= 1) res
        else
          primes match {
            case h #:: _ if (n % h == 0) => helper(n / h, primes, h :: res)
            case h #:: t if (n % h != 0) => helper(n, t, res)
            case x if x.isEmpty          => res
          }

      helper(n, math.sqrt(n.toDouble).toInt.primes, List[Int]())
    }
  }
  // P32
  def gcd(n1: Int, n2: Int): Int = ???
}
