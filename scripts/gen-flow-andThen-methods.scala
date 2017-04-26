val alphaAll = "abcdefghijklmnopqrstuvwx"

for (i <- 3 to 22) {

  val alpha = alphaAll.take(i)
  val head = alpha.head
  val tail = alpha.tail
  val tailUpperComma = tail.toUpperCase.mkString(", ")

  val funs =
    tail.zipWithIndex.map { case (e: Char, idx: Int) =>
      val idxPlusOne = idx + 1
      "fun" + idxPlusOne + ": Flow1[A, " + e.toString.toUpperCase + "]"
    }.mkString(", ")

  val invocations =
    tail.zipWithIndex.map { case (e: Char, idx: Int) =>
      val idxPlusOne = idx + 1
      "            val (res" + idxPlusOne + ", s" + idxPlusOne + ") = fun" + idxPlusOne + ".run(suite, testName, args, res0)"
    }.mkString("\n")

  val combineResult =
    "if (" +
    tail.zipWithIndex.map { case (e: Char, idx: Int) =>
      val idxPlusOne = idx + 1
      "res" + idxPlusOne + ".isDefined"
    }.mkString(" && ") + ") Some((" +
    tail.zipWithIndex.map { case (e: Char, idx: Int) =>
      val idxPlusOne = idx + 1
      "res" + idxPlusOne + ".get"
    }.mkString(", ") + ")) else None"

  val combineStatus =
    "if (" +
      tail.zipWithIndex.map { case (e: Char, idx: Int) =>
        val idxPlusOne = idx + 1
        "s" + idxPlusOne + ".succeeds"
      }.mkString(" && ") + ") SucceededStatus else FailedStatus"

  val cancellations =
    tail.zipWithIndex.map { case (e: Char, idx: Int) =>
      val idxPlusOne = idx + 1
      "            fun" + idxPlusOne + ".cancel(suite, args)"
    }.mkString("\n")

  val testNames =
    tail.zipWithIndex.map { case (e: Char, idx: Int) =>
      val idxPlusOne = idx + 1
      "fun" + idxPlusOne + ".testNames"
    }.mkString(" ++ ")

  println(
    s"""def andThen[$tailUpperComma]($funs): Flow0[($tailUpperComma)] =
       |    new Flow0[($tailUpperComma)] {
       |      def testNames: Set[String] = thisNode.testNames ++ $testNames
       |      override def run(suite: Suite, testName: Option[String], args: Args): (Option[($tailUpperComma)], Status) = {
       |        val (res0, status) = thisNode.run(suite, testName, args)
       |        res0 match {
       |          case Some(res0) =>
       |$invocations
       |
       |            val retV = $combineResult
       |            val retS = $combineStatus
       |
       |            (retV, retS)
       |
       |          case None =>
       |$cancellations
       |            (None, status)
       |        }
       |      }
       |    }
    """.stripMargin)

}

println("================================================================================================")

for (i <- 4 to 24) {

  val alpha = alphaAll.take(i)
  val first2 = alpha.take(2)
  val first = first2.head
  val second = first2.last
  val tail = alpha.drop(2)
  val tailUpperComma = tail.toUpperCase.mkString(", ")

  val funs =
    tail.zipWithIndex.map { case (e: Char, idx: Int) =>
      val idxPlusOne = idx + 1
      "fun" + idxPlusOne + ": Flow1[B, " + e.toString.toUpperCase + "]"
    }.mkString(", ")

  val invocations =
    tail.zipWithIndex.map { case (e: Char, idx: Int) =>
      val idxPlusOne = idx + 1
      "            val (res" + idxPlusOne + ", s" + idxPlusOne + ") = fun" + idxPlusOne + ".run(suite, testName, args, res0)"
    }.mkString("\n")

  val combineResult =
    "if (" +
      tail.zipWithIndex.map { case (e: Char, idx: Int) =>
        val idxPlusOne = idx + 1
        "res" + idxPlusOne + ".isDefined"
      }.mkString(" && ") + ") Some((" +
      tail.zipWithIndex.map { case (e: Char, idx: Int) =>
        val idxPlusOne = idx + 1
        "res" + idxPlusOne + ".get"
      }.mkString(", ") + ")) else None"

  val combineStatus =
    "if (" +
      tail.zipWithIndex.map { case (e: Char, idx: Int) =>
        val idxPlusOne = idx + 1
        "s" + idxPlusOne + ".succeeds"
      }.mkString(" && ") + ") SucceededStatus else FailedStatus"

  val cancellations =
    tail.zipWithIndex.map { case (e: Char, idx: Int) =>
      val idxPlusOne = idx + 1
      "            fun" + idxPlusOne + ".cancel(suite, args)"
    }.mkString("\n")

  val testNames =
    tail.zipWithIndex.map { case (e: Char, idx: Int) =>
      val idxPlusOne = idx + 1
      "fun" + idxPlusOne + ".testNames"
    }.mkString(" ++ ")

  println(
    s"""def andThen[$tailUpperComma]($funs): Flow1[A, ($tailUpperComma)] =
       |    new Flow1[A, ($tailUpperComma)] {
       |      def testNames: Set[String] = self.testNames ++ $testNames
       |      override def cancel(suite: Suite, args: Args): Unit = {
       |        self.cancel(suite, args)
       |$cancellations
       |      }
       |      override def run(suite: Suite, testName: Option[String], args: Args, input: A): (Option[($tailUpperComma)], Status) = {
       |        val (res0, status) = self.run(suite, testName, args, input)
       |        res0 match {
       |          case Some(res0) =>
       |$invocations
       |
       |            val retV = $combineResult
       |            val retS = $combineStatus
       |
       |            (retV, retS)
       |
       |          case None =>
       |$cancellations
       |            (None, status)
       |        }
       |      }
       |    }
    """.stripMargin)

}