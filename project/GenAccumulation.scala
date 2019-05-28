import java.io.File
import java.io.BufferedWriter
import java.io.FileWriter
import scala.io.Source

object GenAccumulation {

  def AccumulationLowPriorityImplicits(scalaVersion: String): String =
    if (scalaVersion startsWith "2.13")
      """trait AccumulationLowPriorityImplicits {
      |
      |  /**
      |    * Implicitly converts a <em>covariant</em> <code>GenTraversableOnce</code> containing accumulating <code>Or</code>s to an instance of
      |    * <a href="Accumulation$$Combinable.html"><code>Combinable</code></a>, which
      |    * enables the <code>combined</code> method to be invoked on it.
      |    *
      |    * <p>
      |    * For more information and examples, see the <a href="Or.html#usingCombined">Using <code>combined</code></a> section of the main documentation for class <code>Or</code>.
      |    * </p>
      |    */
      |  implicit def convertGenTraversableOnceToCombinable[G, ERR, EVERY[b] <: Every[b], TRAVONCE[+e] <: GenTraversableOnce[e]](xs: TRAVONCE[G Or EVERY[ERR]])(implicit cbf: CanBuildFrom[TRAVONCE[G Or EVERY[ERR]], G, TRAVONCE[G]]): Combinable[G, ERR, TRAVONCE] =
      |    new Combinable[G, ERR, TRAVONCE] {
      |
      |      def combined: TRAVONCE[G] Or Every[ERR] = {
      |        // So now I have an empty builder
      |        val emptyTRAVONCEOfGBuilder: Builder[G, TRAVONCE[G]] = cbf(xs)
      |        // So now I want to foldLeft across my TRAVONCE[G Or EVERY[ERR]], starting with an empty TRAVONCEOfGBuilder, and each step along the way, I'll
      |        // += into the builder, what? Oh I get it. The result type of my foldLeft needs to be Builder[Seq[G]] Or Every[ERR]
      |        val tempOr: Builder[G, TRAVONCE[G]] Or Every[ERR] =
      |        xs.foldLeft((Good(emptyTRAVONCEOfGBuilder): Builder[G, TRAVONCE[G]] Or Every[ERR])) { (accumulator: Builder[G, TRAVONCE[G]] Or Every[ERR],  nextElem: G Or Every[ERR]) =>
      |          (accumulator, nextElem) match {
      |            case (Good(bldr), Good(ele)) => Good(bldr += ele)
      |            case (Good(_), Bad(err)) => Bad(err)
      |            case (Bad(errA), Bad(errB)) => Bad(errA ++ errB)
      |            case (Bad(errA), Good(_)) => Bad(errA)
      |          }
      |        }
      |        tempOr map (_.result)
      |      }
      |    }
      |
      |  implicit def convertGenTraversableOnceToCombinable3[E, TRAVONCE[+e] <: GenTraversableOnce[e]](xs: TRAVONCE[Bad[Every[E]]]): Combinable[Nothing, E, TRAVONCE] =
      |    new Combinable[Nothing, E, TRAVONCE] {
      |      override def combined: Or[TRAVONCE[Nothing], Every[E]] = {
      |        val either: Either[Every[E], TRAVONCE[Nothing]] =
      |          if (xs.isEmpty)
      |            Right(Vector.empty[Nothing].asInstanceOf[TRAVONCE[Nothing]])
      |          else {
      |            val i = xs.toIterable
      |            Left(
      |              (i.tail.foldLeft(i.head.b) { case (res, ele) =>
      |                res ++ ele.b
      |              }.asInstanceOf[Every[E]])
      |            )
      |          }
      |
      |        Or.from(either)
      |      }
      |    }
      |
      |}
      """.stripMargin
    else
      ""

  def extends_AccumulationLowPriorityImplicits(scalaVersion: String): String =
    if (scalaVersion startsWith "2.13")
      "extends AccumulationLowPriorityImplicits"
    else
      ""

  def combinable_traversable_implicits(scalaVersion: String): String =
    if (scalaVersion startsWith "2.13")
      """  implicit def convertGenTraversableOnceToCombinable2[E, TRAVONCE[+e] <: Iterable[e]](xs: TRAVONCE[Good[E]])(implicit cbf: CanBuildFrom[TRAVONCE[Good[E]], Good[E], TRAVONCE[Good[E]]]): Combinable[E, Nothing, TRAVONCE] =
        |    new Combinable[E, Nothing, TRAVONCE] {
        |      override def combined: Or[TRAVONCE[E], Every[Nothing]] = {
        |        // So now I have an empty builder
        |        val emptyTRAVONCEOfGBuilder: Builder[Good[E], TRAVONCE[Good[E]]] = cbf(xs)
        |        Or.from(
        |          Right(
        |            (xs.foldLeft(emptyTRAVONCEOfGBuilder) { case (res, ele) =>
        |              res += ele
        |            }).mapResult(_.map(_.get)).result().asInstanceOf[TRAVONCE[E]]
        |          )
        |        )
        |      }
        |
        |    }
        |
        |  /**
        |   * Implicitly converts a <code>Set</code> containing accumulating <code>Or</code>s whose <code>Good</code> type is inferred as <code>Nothing</code> to an
        |   * instance of <a href="Accumulation$$Combinable.html"><code>Combinable</code></a>, which
        |   * enables the <code>combined</code> method to be invoked on it.
        |   *
        |   * <p>
        |   * Note: This implicit is required for <code>Set</code>s because although <code>Set</code>s are <code>GenTraversableOnce</code>s, they aren't covariant, so
        |   * the implicit conversion provided by <code>convertGenTraversableOnceToCombinableNothing</code> will not be applied, because it only works on <em>covariant</em>
        |   * <code>GenTraversableOnce</code>s.
        |   * </p>
        |   */
        |  implicit def convertGenSetOnceToCombinable2[E, SET[e] <: GenSet[e]](xs: SET[Good[E]])(implicit cbf: CanBuildFrom[SET[Good[E]], Good[E], SET[Good[E]]]): Combinable[E, Nothing, SET] =
        |    new Combinable[E, Nothing, SET] {
        |      override def combined: Or[SET[E], Every[Nothing]] = {
        |        // So now I have an empty builder
        |        val emptyTRAVONCEOfGBuilder: Builder[Good[E], SET[Good[E]]] = cbf(xs)
        |        Or.from(
        |          Right(
        |            (xs.foldLeft(emptyTRAVONCEOfGBuilder) { case (res, ele) =>
        |              res += ele
        |            }).mapResult(_.map(_.get)).result().asInstanceOf[SET[E]]
        |          )
        |        )
        |      }
        |
        |    }
        |
        |  /**
        |    * Implicitly converts a <code>Set</code> containing accumulating <code>Or</code>s whose <code>Bad</code> type is inferred as <code>Nothing</code> to an
        |    * instance of <a href="Accumulation$$Combinable.html"><code>Combinable</code></a>, which
        |    * enables the <code>combined</code> method to be invoked on it.
        |    *
        |    * <p>
        |    * Note: This implicit is required for <code>Set</code>s because although <code>Set</code>s are <code>GenTraversableOnce</code>s, they aren't covariant, so
        |    * the implicit conversion provided by <code>convertGenSetToCombinableNothing</code> will not be applied, because it only works on <em>covariant</em>
        |    * <code>GenTraversableOnce</code>s.
        |    * </p>
        |    */
        |  implicit def convertGenSetOnceToCombinable3[E, SET[e] <: GenSet[e], EVERY[f] <: Every[f]](xs: SET[Bad[EVERY[E]]]): Combinable[Nothing, E, SET] =
        |    new Combinable[Nothing, E, SET] {
        |      override def combined: Or[SET[Nothing], EVERY[E]] = {
        |        val either: Either[EVERY[E], SET[Nothing]] =
        |          if (xs.isEmpty)
        |            Right(Vector.empty[Nothing].asInstanceOf[SET[Nothing]])
        |          else {
        |            val i = xs.toIterable
        |            Left(
        |              (i.tail.foldLeft(i.head.b) { case (res, ele) =>
        |                (res ++ ele.b).asInstanceOf[EVERY[E]]
        |              })
        |            )
        |          }
        |
        |        Or.from(either)
        |      }
        |    }
      """.stripMargin
    else
      """  /**
        |   * Implicitly converts a <em>covariant</em> <code>GenTraversableOnce</code> containing accumulating <code>Or</code>s to an instance of
        |   * <a href="Accumulation$$Combinable.html"><code>Combinable</code></a>, which
        |   * enables the <code>combined</code> method to be invoked on it.
        |   *
        |   * <p>
        |   * For more information and examples, see the <a href="Or.html#usingCombined">Using <code>combined</code></a> section of the main documentation for class <code>Or</code>.
        |   * </p>
        |   */
        |  implicit def convertGenTraversableOnceToCombinable[G, ERR, EVERY[b] <: Every[b], TRAVONCE[+e] <: GenTraversableOnce[e]](xs: TRAVONCE[G Or EVERY[ERR]])(implicit cbf: CanBuildFrom[TRAVONCE[G Or EVERY[ERR]], G, TRAVONCE[G]]): Combinable[G, ERR, TRAVONCE] =
        |    new Combinable[G, ERR, TRAVONCE] {
        |
        |      def combined: TRAVONCE[G] Or Every[ERR] = {
        |        // So now I have an empty builder
        |        val emptyTRAVONCEOfGBuilder: Builder[G, TRAVONCE[G]] = cbf(xs)
        |        // So now I want to foldLeft across my TRAVONCE[G Or EVERY[ERR]], starting with an empty TRAVONCEOfGBuilder, and each step along the way, I'll
        |        // += into the builder, what? Oh I get it. The result type of my foldLeft needs to be Builder[Seq[G]] Or Every[ERR]
        |        val tempOr: Builder[G, TRAVONCE[G]] Or Every[ERR] =
        |          xs.foldLeft((Good(emptyTRAVONCEOfGBuilder): Builder[G, TRAVONCE[G]] Or Every[ERR])) { (accumulator: Builder[G, TRAVONCE[G]] Or Every[ERR],  nextElem: G Or Every[ERR]) =>
        |            (accumulator, nextElem) match {
        |              case (Good(bldr), Good(ele)) => Good(bldr += ele)
        |              case (Good(_), Bad(err)) => Bad(err)
        |              case (Bad(errA), Bad(errB)) => Bad(errA ++ errB)
        |              case (Bad(errA), Good(_)) => Bad(errA)
        |            }
        |          }
        |        tempOr map (_.result)
        |      }
        |    }
        |
        |  implicit def convertGenTraversableOnceToCombinable2[G, EVERY[b] <: Every[b], TRAVONCE[+e] <: GenTraversableOnce[e]](xs: TRAVONCE[G Or EVERY[Nothing]])(implicit cbf: CanBuildFrom[TRAVONCE[G Or EVERY[Nothing]], G, TRAVONCE[G]]): Combinable[G, Nothing, TRAVONCE] =
        |    new Combinable[G, Nothing, TRAVONCE] {
        |
        |      def combined: TRAVONCE[G] Or Every[Nothing] = {
        |        // So now I have an empty builder
        |        val emptyTRAVONCEOfGBuilder: Builder[G, TRAVONCE[G]] = cbf(xs)
        |        // So now I want to foldLeft across my TRAVONCE[G Or EVERY[ERR]], starting with an empty TRAVONCEOfGBuilder, and each step along the way, I'll
        |        // += into the builder, what? Oh I get it. The result type of my foldLeft needs to be Builder[Seq[G]] Or Every[ERR]
        |        val tempOr: Builder[G, TRAVONCE[G]] Or Every[Nothing] =
        |          xs.foldLeft((Good(emptyTRAVONCEOfGBuilder): Builder[G, TRAVONCE[G]] Or Every[Nothing])) { (accumulator: Builder[G, TRAVONCE[G]] Or Every[Nothing],  nextElem: G Or Every[Nothing]) =>
        |            (accumulator, nextElem) match {
        |              case (Good(bldr), Good(ele)) => Good(bldr += ele)
        |              case (Good(_), Bad(err)) => Bad(err)
        |              case (Bad(errA), Bad(errB)) => Bad(errA ++ errB)
        |              case (Bad(errA), Good(_)) => Bad(errA)
        |            }
        |          }
        |        tempOr map (_.result)
        |      }
        |    }
        |
        |  /**
        |   * Implicitly converts a <em>covariant</em> <code>GenTraversableOnce</code> containing accumulating <code>Or</code>s whose inferred <code>Good</code> type
        |   * is inferred as <code>Nothing</code> to an instance of
        |   * <a href="Accumulation$$Combinable.html"><code>Combinable</code></a>, which
        |   * enables the <code>combined</code> method to be invoked on it.
        |   */
        |  implicit def convertGenTraversableOnceToCombinableNothing[ERR, EVERY[b] <: Every[b], TRAVONCE[+e] <: GenTraversableOnce[e]](xs: TRAVONCE[Nothing Or EVERY[ERR]])(implicit cbf: CanBuildFrom[TRAVONCE[Nothing Or EVERY[ERR]], Nothing, TRAVONCE[Nothing]]): Combinable[Nothing, ERR, TRAVONCE] = convertGenTraversableOnceToCombinable[Nothing, ERR, EVERY, TRAVONCE](xs)(cbf)
        |
        |  /**
        |   * Implicitly converts a <code>Set</code> containing accumulating <code>Or</code>s whose <code>Good</code> type is inferred as <code>Nothing</code> to an
        |   * instance of <a href="Accumulation$$Combinable.html"><code>Combinable</code></a>, which
        |   * enables the <code>combined</code> method to be invoked on it.
        |   *
        |   * <p>
        |   * Note: This implicit is required for <code>Set</code>s because although <code>Set</code>s are <code>GenTraversableOnce</code>s, they aren't covariant, so
        |   * the implicit conversion provided by <code>convertGenTraversableOnceToCombinableNothing</code> will not be applied, because it only works on <em>covariant</em>
        |   * <code>GenTraversableOnce</code>s.
        |   * </p>
        |   */
        |  implicit def convertGenSetToCombinableNothing[ERR, X, EVERY[b] <: Every[b], SET[e] <: GenSet[e]](xs: SET[X with (Nothing Or EVERY[ERR])])(implicit cbf: CanBuildFrom[SET[X with (Nothing Or EVERY[ERR])], Nothing, SET[Nothing]]): Combinable[Nothing, ERR, SET] = convertGenSetToCombinable[Nothing, ERR, X, EVERY, SET](xs)(cbf)
      """.stripMargin

  def transformLine(line: String, scalaVersion: String): String = {
    line
      .replaceAllLiterally("$$AccumulationLowPriorityImplicits$$", AccumulationLowPriorityImplicits(scalaVersion))
      .replaceAllLiterally("$$extends_AccumulationLowPriorityImplicits$$", extends_AccumulationLowPriorityImplicits(scalaVersion))
      .replaceAllLiterally("$$combinable_traversable_implicits$$", combinable_traversable_implicits(scalaVersion))
  }

  private def copyFile(sourceFile: File, destFile: File, scalaVersion: String): File = {
    destFile.getParentFile.mkdirs()
    val destWriter = new BufferedWriter(new FileWriter(destFile))
    try {
      val lines = Source.fromFile(sourceFile).getLines.toList
      for (line <- lines) {
        destWriter.write(transformLine(line, scalaVersion))
        destWriter.newLine()
      }
      destFile
    }
    finally {
      destWriter.flush()
      destWriter.close()
      println("Generated " + destFile.getAbsolutePath)
    }
  }

  def genMain(targetDir: File, version: String, scalaVersion: String): Seq[File] =
    Seq(
      copyFile(new File("project/templates/Accumulation.scala.template"), new File(targetDir, "Accumulation.scala"), scalaVersion)
    )

}