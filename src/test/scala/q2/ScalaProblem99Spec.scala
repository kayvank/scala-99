package q2

import org.specs2.mutable.Specification
import ScalaProblem99._

class ScalaProblem99Specs extends Specification {
  "Specifications for Scala 99 problems" title

  "consective duplicates specs" >> {
    val tstList = List(1,2,1,1,2,2,3,3,4,1)
    val computed = deDups(tstList)
    println(s"-- deDups computed = ${computed}")
    computed === List(1,2,1,2,3,4,1)
  }

  "compress duplicates specs" >> {
    val tstList = List(1,2,1,1,2,2,3,3,4,1)
    val computed = compressList(tstList)
    println(s"-- deDups computed = ${computed}")
    computed === List(1,2,1,2,3,4,1)
  }

  " deDup using fold  specs" >> {
    val tstList = List(1,2,1,1,2,2,3,3,4,1)
    val computed = dedupFold(tstList)
    println(s"-- deDups computed = ${computed}")
    computed === List(1,2,1,2,3,4,1)
  }

  "packing a list specs" >> {
    val tstList = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    val computed = packList(tstList)
    println(s"-- packedList computed = ${computed}")
    computed ===  List(
      List('a, 'a, 'a, 'a),
      List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
  }

  "packing a list with scan specs" >> {
    val tstList = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    val computed = packWithScan(tstList)
    println(s"-- packedList computed = ${computed}")
    computed ===  List(
      List('a, 'a, 'a, 'a),
      List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
  }

  "encode list specs" >> {
    encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) ===
      List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
  }

  "decode list specs" >> {
    decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) ===
      List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
  }

  "duplicate N times list specs" >> {
    duplicateN(3, List('a, 'b, 'c, 'c, 'd)) ===
      List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
  }

  "drop every N list specs" >> {
    drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) ===
      List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
  }

  "split list specs" >> {
    split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) ===
      (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  }

}
