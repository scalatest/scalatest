val alphaAll = "abcdefghijklmnopqrstuvw"

for (i <- 3 to 22) {

  /*
   * implicit def tuple2Generator[A, B](implicit genOfA: Generator[A], genOfB: Generator[B]): Generator[(A, B)] =
   * new GeneratorFor2[A, B, (A, B)]((a: A, b: B) => (a, b), (c: (A, B)) => c)(genOfA, genOfB)
   */

  /*val alpha = alphaAll.take(i)
  val alphaUpper = alpha.toUpperCase.mkString(", ")
  val alphaLower = alpha.mkString(", ")
  val gensWithTypes = alpha.toUpperCase.map(a => "genOf" + a + ": Generator[" + a + "]").mkString(", ")
  val gens = alpha.toUpperCase.map(a => "genOf" + a).mkString(", ")
  val alphaWithTypes = alpha.map(a => a + ": " + a.toString.toUpperCase).mkString(", ")
  val alphaPlusOne = alphaAll(i)*/

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

  //println("implicit def tuple" + i + "Generator[" + alphaUpper + "](implicit " + gensWithTypes + "): Generator[(" + alphaUpper + ")] = \n" +
    //"new GeneratorFor" + i + "[" + alphaUpper + ", (" + alphaUpper + ")]((" + alphaWithTypes + ") => (" + alphaLower + "), (" + alphaPlusOne + ": (" + alphaUpper + ")) => " + alphaPlusOne + ")(" + gens + ")")

}