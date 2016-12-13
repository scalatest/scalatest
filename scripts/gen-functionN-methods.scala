val alphaAll = "abcdefghijklmnopqrstuvw"

for (i <- 2 to 22) {

  val alpha = alphaAll.take(i)
  val alphaPlusOne = alphaAll(i)
  val alphaUpper = alpha.toUpperCase.mkString(", ")
  val alphaLower = alpha.mkString(", ")
  val alphaPlusOneUpper = alphaPlusOne.toString.toUpperCase
  val typeTypeOfAll = alpha.toUpperCase.map(a => "typeTagOf" + a + ": TypeTag[" + a + "]").mkString(", ") + ", " + "typeTagOf" + alphaPlusOneUpper + ": TypeTag[" + alphaPlusOneUpper + "]"
  val alphaWithTypes = alpha.map(a => a + ": " + a.toString.toUpperCase).mkString(", ")
  val typeOfAlphaDcls = alpha.toUpperCase.map(a => "val typeOf" + a + " = typeTagOf" + a + ".tpe").mkString("\n")
  val typeOfAlphaPlusOneDcls = "val typeOf" + alphaPlusOneUpper + " = typeTagOf" + alphaPlusOneUpper + ".tpe"
  val alphaWithDollarTypeOf = alpha.map(a => a + ": $typeOf" + a.toString.toUpperCase).mkString(", ")

  println("implicit def function" + i + "Generator[" + alphaUpper + ", " + alphaPlusOneUpper + "](implicit genOf" + alphaPlusOneUpper + ": Generator[" + alphaPlusOneUpper + "], " + typeTypeOfAll+ "): Generator[(" + alphaUpper + ") => " + alphaPlusOneUpper + "] = {\n" +
          "  new Generator[(" + alphaUpper + ") => " + alphaPlusOneUpper + "] {\n" +
          "    def next(size: Int, edges: List[(" + alphaUpper + ") => " + alphaPlusOneUpper + "], rnd: Randomizer): ((" + alphaUpper + ") => " + alphaPlusOneUpper + ", List[(" + alphaUpper + ") => " + alphaPlusOneUpper + "], Randomizer) = {\n" +
          "      val intToIntGen: Generator[Int => Int] = function1IntToIntGenerator\n" +
          "      val (intToInt, _, rnd) = intToIntGen.next(10, Nil, rnd)\n\n" +
          "      object " + alpha.toUpperCase + "To" + alphaPlusOneUpper + " extends ((" + alphaUpper + ") => " + alphaPlusOneUpper + ") {\n" +
          "        def apply(" + alphaWithTypes + "): " + alphaPlusOneUpper + " = org.scalatest.prop.valueOf[" + alphaPlusOneUpper + "](" + alphaLower + ", intToInt)\n" +
          "        override def toString = {\n" +
          "          " + typeOfAlphaDcls + "\n" +
          "          " + typeOfAlphaPlusOneDcls + "\n" +
          "          val intToIntName: String = \n" +
          "            intToInt match {\n" +
          "              case prf: PrettyFunction1[_, _] => prf.simpleName\n" +
          "              case _ => intToInt.toString\n" +
          "            }\n" +
          "          s\"(" + alphaWithDollarTypeOf + ") => org.scalatest.prop.valueOf[$typeOf" + alphaPlusOneUpper + "](" + alphaLower + ", $intToIntName)\n" +
          "        }\n" +
          "      }\n\n" +
          "      (" + alpha.toUpperCase + "To" + alphaPlusOneUpper + ", Nil, rnd1)\n" +
          "    }\n" +
          "  }\n" +
          "}\n")

}