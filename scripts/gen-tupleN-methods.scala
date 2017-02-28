val alphaAll = "abcdefghijklmnopqrstuvw"

for (i <- 2 to 22) {

  /*
   * implicit def tuple2Generator[A, B](implicit genOfA: Generator[A], genOfB: Generator[B]): Generator[(A, B)] =
   * new GeneratorFor2[A, B, (A, B)]((a: A, b: B) => (a, b), (c: (A, B)) => c)(genOfA, genOfB)
   */

  val alpha = alphaAll.take(i)
  val alphaUpper = alpha.toUpperCase.mkString(", ")
  val alphaLower = alpha.mkString(", ")
  val gensWithTypes = alpha.toUpperCase.map(a => "genOf" + a + ": Generator[" + a + "]").mkString(", ")
  val gens = alpha.toUpperCase.map(a => "genOf" + a).mkString(", ")
  val alphaWithTypes = alpha.map(a => a + ": " + a.toString.toUpperCase).mkString(", ")
  val alphaPlusOne = alphaAll(i)

  println("implicit def tuple" + i + "Generator[" + alphaUpper + "](implicit " + gensWithTypes + "): Generator[(" + alphaUpper + ")] = \n" +
          "new GeneratorFor" + i + "[" + alphaUpper + ", (" + alphaUpper + ")]((" + alphaWithTypes + ") => (" + alphaLower + "), (" + alphaPlusOne + ": (" + alphaUpper + ")) => " + alphaPlusOne + ")(" + gens + ")")

}