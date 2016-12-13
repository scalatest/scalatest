val alphaAll = "abcdefghijklmnopqrstuvw"
val range = 2 to 22

for (i <- range) {
  val alpha = alphaAll.take(i)
  val alphaPlusOne = alphaAll(i)
  val alphaLower = alpha.mkString(", ")
  val alphaWithAnyTypes = alpha.map(a => a + ": Any").mkString(", ")
  val hashCodeImpl =
    alpha.tail.foldLeft("    37 + a.hashCode") { case (current, a) =>
      "    37 * (\n" +
      current.split('\n').map(l => "  " + l).mkString("\n") + "\n" +
      "    ) + " + a + ".hashCode"
    }.substring(4)

  println("def valueOf[" + alphaPlusOne.toString.toUpperCase + "](multiplier: Int, " + alphaWithAnyTypes + ")(implicit genOf" + alphaPlusOne.toString.toUpperCase + ": Generator[" + alphaPlusOne.toString.toUpperCase + "]): " + alphaPlusOne.toString.toUpperCase + " = {\n" +
          "  def combinedHashCode(" + alphaWithAnyTypes + "): Int = \n" +
          "    " + hashCodeImpl + "\n" +
          "  val seed = combinedHashCode(" + alphaLower + ").toLong * multiplier\n" +
          "  val rnd = Randomizer(seed)\n" +
          "  val (size, nextRnd) = rnd.chooseInt(1, 20)\n" +
          "  val (result, _, _) = genOf" + alphaPlusOne.toString.toUpperCase + ".next(size, Nil, nextRnd)\n" +
          "  result\n" +
          "}")
}

println("==================================================================================")

for (i <- range) {

  val alpha = alphaAll.take(i)
  val alphaPlusOne = alphaAll(i)
  val alphaUpper = alpha.toUpperCase.mkString(", ")
  val alphaLower = alpha.mkString(", ")
  val alphaPlusOneUpper = alphaPlusOne.toString.toUpperCase
  val typeTypeOfAll = alpha.toUpperCase.map(a => "typeTagOf" + a + ": TypeTag[" + a + "]").mkString(", ") + ", " + "typeTagOf" + alphaPlusOneUpper + ": TypeTag[" + alphaPlusOneUpper + "]"
  val alphaWithTypes = alpha.map(a => a + ": " + a.toString.toUpperCase).mkString(", ")
  val typeOfAlphaDcls = alpha.toUpperCase.map(a => "val typeOf" + a + " = typeTagOf" + a + ".tpe").mkString("\n          ")
  val typeOfAlphaPlusOneDcls = "val typeOf" + alphaPlusOneUpper + " = typeTagOf" + alphaPlusOneUpper + ".tpe"
  val alphaWithDollarTypeOf = alpha.map(a => a + ": $typeOf" + a.toString.toUpperCase).mkString(", ")

  println("implicit def function" + i + "Generator[" + alphaUpper + ", " + alphaPlusOneUpper + "](implicit genOf" + alphaPlusOneUpper + ": Generator[" + alphaPlusOneUpper + "], " + typeTypeOfAll+ "): Generator[(" + alphaUpper + ") => " + alphaPlusOneUpper + "] = {\n" +
          "  new Generator[(" + alphaUpper + ") => " + alphaPlusOneUpper + "] {\n" +
          "    def next(size: Int, edges: List[(" + alphaUpper + ") => " + alphaPlusOneUpper + "], rnd: Randomizer): ((" + alphaUpper + ") => " + alphaPlusOneUpper + ", List[(" + alphaUpper + ") => " + alphaPlusOneUpper + "], Randomizer) = {\n" +
          "      val first1000PrimesGen: Generator[Int] = first1000Primes\n" +
          "      val (prime, _, rnd1) = first1000PrimesGen.next(10, Nil, rnd)\n" +
          "      val multiplier = if (prime == 2) 1 else prime\n\n" +
          "      object " + alpha.toUpperCase + "To" + alphaPlusOneUpper + " extends ((" + alphaUpper + ") => " + alphaPlusOneUpper + ") {\n" +
          "        def apply(" + alphaWithTypes + "): " + alphaPlusOneUpper + " = org.scalatest.prop.valueOf[" + alphaPlusOneUpper + "](multiplier, " + alphaLower + ")\n" +
          "        override def toString = {\n" +
          "          " + typeOfAlphaDcls + "\n" +
          "          " + typeOfAlphaPlusOneDcls + "\n" +
          "          s\"(" + alphaWithDollarTypeOf + ") => org.scalatest.prop.valueOf[$typeOf" + alphaPlusOneUpper + s"]($$multiplier, " + alphaLower + ")\"\n" +
          "        }\n" +
          "      }\n\n" +
          "      (" + alpha.toUpperCase + "To" + alphaPlusOneUpper + ", Nil, rnd1)\n" +
          "    }\n" +
          "  }\n" +
          "}\n")

}

println("==================================================================================")

for (i <- range) {
  val alpha = alphaAll.take(i)
  val alphaPlusOne = alphaAll(i)
  val alphaUpper = alpha.toUpperCase.mkString(", ")
  val alphaPlusOneUpper = alphaPlusOne.toString.toUpperCase
  val typeTypeOfAll = alpha.toUpperCase.map(a => "typeTagOf" + a + ": TypeTag[" + a + "]").mkString(", ") + ", " + "typeTagOf" + alphaPlusOneUpper + ": TypeTag[" + alphaPlusOneUpper + "]"

  println("def function" + i + "s[" + alphaUpper + ", " + alphaPlusOneUpper + "](implicit genOf" + alphaPlusOneUpper + ": Generator[" + alphaPlusOneUpper + "], " + typeTypeOfAll + "): Generator[(" + alphaUpper + ") => " + alphaPlusOneUpper + "] = \n" +
          "  Generator.function" + i + "Generator[" + alphaUpper + ", " + alphaPlusOneUpper + "]")
}
