package org.scalatest
package prop

import java.lang.annotation.AnnotationFormatError
import java.nio.charset.CoderMalfunctionError
import javax.xml.parsers.FactoryConfigurationError
import javax.xml.transform.TransformerFactoryConfigurationError

import org.scalatest.FailureMessages._
import org.scalatest.SharedHelpers._
import org.scalatest._
import org.scalatest.exceptions.TestCanceledException

/**
 * Separate non-generated tests, for completeness
 */
class NonGeneratedTableDrivenPropertyChecksSpec extends Spec with Matchers with TableDrivenPropertyChecks with OptionValues {

  object `forEvery/1 ` {
    def colFun[A](s: Set[A]): TableFor1[A] = {

      val table = Table(("column1"), s.toSeq: _*)
      table
    }

    def `should pass when all elements passed` {
      val col = colFun(Set(1, 2, 3))
      forEvery(col) { e => e should be < 4}
    }

    def `should throw TestFailedException with correct stack depth and message when at least one element failed` {
      val col = colFun(Set(1, 2, 3))
      val e = intercept[exceptions.TestFailedException] {
        forEvery(col) { e => e should not equal 2}
      }
      e.failedCodeFileName should be(Some("NonGeneratedTableDrivenPropertyChecksSpec.scala"))
      e.failedCodeLineNumber should be(Some(thisLineNumber - 3))
      val index = getIndex(col, 2)
      e.message.value should be(s"forEvery failed, because: \n" +
        s"  org.scalatest.exceptions.TableDrivenPropertyCheckFailedException: TestFailedException was thrown during property evaluation. (NonGeneratedTableDrivenPropertyChecksSpec.scala:${thisLineNumber - 6})\n" +
        s"    Message: 2 equaled 2\n" +
        s"    Location: (NonGeneratedTableDrivenPropertyChecksSpec.scala:${thisLineNumber - 8})\n" +
        s"    Occurred at table row 1 (zero based, not counting headings), which had values (\n" +
        s"    column1 = 2  )"
      )
    }

    def `should throw TestFailedException with correct stack depth and message when more than one element failed` {
      val col = colFun(Set(1, 2, 3))
      val e = intercept[exceptions.TestFailedException] {
        forEvery(col) { e => e should be < 2}
      }
      e.failedCodeFileName should be(Some("NonGeneratedTableDrivenPropertyChecksSpec.scala"))
      e.failedCodeLineNumber should be(Some(thisLineNumber - 3))
      val itr = col.toIterator
      val first = getNextNot[Int](itr, _ < 2)
      val firstIndex = getIndex(col, first)
      val second = getNextNot[Int](itr, _ < 2)
      val secondIndex = getIndex(col, second)
      e.message.value should be(s"forEvery failed, because: \n" +
        s"  org.scalatest.exceptions.TableDrivenPropertyCheckFailedException: TestFailedException was thrown during property evaluation. (NonGeneratedTableDrivenPropertyChecksSpec.scala:${thisLineNumber - 10})\n" +
        s"    Message: 2 was not less than 2\n" +
        s"    Location: (NonGeneratedTableDrivenPropertyChecksSpec.scala:${thisLineNumber - 12})\n" +
        s"    Occurred at table row 1 (zero based, not counting headings), which had values (\n" +
        s"    column1 = 2  ), \n" +
        s"  org.scalatest.exceptions.TableDrivenPropertyCheckFailedException: TestFailedException was thrown during property evaluation. (NonGeneratedTableDrivenPropertyChecksSpec.scala:${thisLineNumber - 15})\n" +
        s"    Message: 3 was not less than 2\n" +
        s"    Location: (NonGeneratedTableDrivenPropertyChecksSpec.scala:${thisLineNumber - 17})\n" +
        s"    Occurred at table row 2 (zero based, not counting headings), which had values (\n" +
        s"    column1 = 3  )"
      )
    }

    def `should propagate TestPendingException thrown from assertion` {
      val col = colFun(Set(1, 2, 3))
      intercept[exceptions.TestPendingException] {
        forEvery(col) { e => pending}
      }
    }

    def `should propagate TestCanceledException thrown from assertion` {
      val example = colFun(Set(1, 2, 3))
      intercept[exceptions.TestCanceledException] {
        forEvery(example) { e => cancel}
      }
    }

    def `should not propagate DiscardedEvaluationException thrown from assertion` {
      val col = colFun(Set(1, 2, 3))
      forEvery(col) { e => throw new DiscardedEvaluationException}
    }

    def `should propagate java.lang.annotation.AnnotationFormatError thrown from assertion` {
      val col = colFun(Set(1, 2, 3))
      intercept[AnnotationFormatError] {
        forEvery(col) { e => throw new AnnotationFormatError("test")}
      }
    }

    def `should propagate java.nio.charset.CoderMalfunctionError thrown from assertion` {
      val col = colFun(Set(1, 2, 3))
      intercept[CoderMalfunctionError] {
        forEvery(col) { e => throw new CoderMalfunctionError(new RuntimeException("test"))}
      }
    }

    def `should propagate javax.xml.parsers.FactoryConfigurationError thrown from assertion` {
      val col = colFun(Set(1, 2, 3))
      intercept[FactoryConfigurationError] {
        forEvery(col) { e => throw new FactoryConfigurationError()}
      }
    }

    def `should propagate java.lang.LinkageError thrown from assertion` {
      val col = colFun(Set(1, 2, 3))
      intercept[LinkageError] {
        forEvery(col) { e => throw new LinkageError()}
      }
    }

    def `should propagate java.lang.ThreadDeath thrown from assertion` {
      val col = colFun(Set(1, 2, 3))
      intercept[ThreadDeath] {
        forEvery(col) { e => throw new ThreadDeath()}
      }
    }

    def `should propagate javax.xml.transform.TransformerFactoryConfigurationError thrown from assertion` {
      val col = colFun(Set(1, 2, 3))
      intercept[TransformerFactoryConfigurationError] {
        forEvery(col) { e => throw new TransformerFactoryConfigurationError()}
      }
    }

    def `should propagate java.lang.VirtualMachineError thrown from assertion` {
      val col = colFun(Set(1, 2, 3))
      intercept[VirtualMachineError] {
        forEvery(col) { e => throw new VirtualMachineError() {}}
      }
    }
  }

  object `forEvery/2 ` {
    def colFun[A](s: Set[A]): TableFor2[A, A] = {
      val table = Table(("column1", "column2"), s.map(x => (x, x)).toSeq: _*)
      table
    }

    def `should pass when all elements passed` {
      val col = colFun(Set(1, 2, 3))
      forEvery(col) { (e, _) => e should be < 4}
    }

    def `should throw TestFailedException with correct stack depth and message when at least one element failed` {
      val col = colFun(Set(1, 2, 3))
      val e = intercept[exceptions.TestFailedException] {
        forEvery(col) { (e, _) => e should not equal 2}
      }
      e.failedCodeFileName should be(Some("NonGeneratedTableDrivenPropertyChecksSpec.scala"))
      e.failedCodeLineNumber should be(Some(thisLineNumber - 3))
      val index = getIndex(col, (2, 2))
      e.message.value should be(s"forEvery failed, because: \n" +
        s"  org.scalatest.exceptions.TableDrivenPropertyCheckFailedException: TestFailedException was thrown during property evaluation. (NonGeneratedTableDrivenPropertyChecksSpec.scala:${thisLineNumber - 6})\n" +
        s"    Message: 2 equaled 2\n" +
        s"    Location: (NonGeneratedTableDrivenPropertyChecksSpec.scala:${thisLineNumber - 8})\n" +
        s"    Occurred at table row 1 (zero based, not counting headings), which had values (\n" +
        s"    column1 = 2\n" +
        s"    column2 = 2  )"
      )
    }

    def `should throw TestFailedException with correct stack depth and message when more than one element failed` {
      val col = colFun(Set(1, 2, 3))
      val e = intercept[exceptions.TestFailedException] {
        forEvery(col) { (e, _) => e should be < 2}
      }
      e.failedCodeFileName should be(Some("NonGeneratedTableDrivenPropertyChecksSpec.scala"))
      e.failedCodeLineNumber should be(Some(thisLineNumber - 3))
      val itr = col.toIterator
      val first = getNextNot[(Int, Int)](itr, _._1 < 2)
      val firstIndex = getIndex(col, first)
      val second = getNextNot[(Int, Int)](itr, _._1 < 2)
      val secondIndex = getIndex(col, second)
      e.message.value should be(s"forEvery failed, because: \n" +
        s"  org.scalatest.exceptions.TableDrivenPropertyCheckFailedException: TestFailedException was thrown during property evaluation. (NonGeneratedTableDrivenPropertyChecksSpec.scala:${thisLineNumber - 10})\n" +
        s"    Message: 2 was not less than 2\n" +
        s"    Location: (NonGeneratedTableDrivenPropertyChecksSpec.scala:${thisLineNumber - 12})\n" +
        s"    Occurred at table row 1 (zero based, not counting headings), which had values (\n" +
        s"    column1 = 2\n" +
        s"    column2 = 2  ), \n" +
        s"  org.scalatest.exceptions.TableDrivenPropertyCheckFailedException: TestFailedException was thrown during property evaluation. (NonGeneratedTableDrivenPropertyChecksSpec.scala:${thisLineNumber - 16})\n" +
        s"    Message: 3 was not less than 2\n" +
        s"    Location: (NonGeneratedTableDrivenPropertyChecksSpec.scala:${thisLineNumber - 18})\n" +
        s"    Occurred at table row 2 (zero based, not counting headings), which had values (\n" +
        s"    column1 = 3\n" +
        s"    column2 = 3  )"
      )
    }

    def `should propagate not DiscardedEvaluationException thrown from assertion` {
      val col = colFun(Set(1, 2, 3))
      forEvery(col) { (_, _) => throw new DiscardedEvaluationException}
    }

    def `should propagate TestPendingException thrown from assertion` {
      val col = colFun(Set(1, 2, 3))
      intercept[exceptions.TestPendingException] {
        forEvery(col) { (e, _) => pending}
      }
    }

    def `should propagate TestCanceledException thrown from assertion` {
      val example = colFun(Set(1, 2, 3))
      intercept[exceptions.TestCanceledException] {
        forEvery(example) { (e, _) => cancel}
      }
    }

    def `should propagate java.lang.annotation.AnnotationFormatError thrown from assertion` {
      val col = colFun(Set(1, 2, 3))
      intercept[AnnotationFormatError] {
        forEvery(col) { (e, _) => throw new AnnotationFormatError("test")}
      }
    }

    def `should propagate java.nio.charset.CoderMalfunctionError thrown from assertion` {
      val col = colFun(Set(1, 2, 3))
      intercept[CoderMalfunctionError] {
        forEvery(col) { (e, _) => throw new CoderMalfunctionError(new RuntimeException("test"))}
      }
    }

    def `should propagate javax.xml.parsers.FactoryConfigurationError thrown from assertion` {
      val col = colFun(Set(1, 2, 3))
      intercept[FactoryConfigurationError] {
        forEvery(col) { (e, _) => throw new FactoryConfigurationError()}
      }
    }

    def `should propagate java.lang.LinkageError thrown from assertion` {
      val col = colFun(Set(1, 2, 3))
      intercept[LinkageError] {
        forEvery(col) { (e, _) => throw new LinkageError()}
      }
    }

    def `should propagate java.lang.ThreadDeath thrown from assertion` {
      val col = colFun(Set(1, 2, 3))
      intercept[ThreadDeath] {
        forEvery(col) { (e, _) => throw new ThreadDeath()}
      }
    }

    def `should propagate javax.xml.transform.TransformerFactoryConfigurationError thrown from assertion` {
      val col = colFun(Set(1, 2, 3))
      intercept[TransformerFactoryConfigurationError] {
        forEvery(col) { (e, _) => throw new TransformerFactoryConfigurationError()}
      }
    }

    def `should propagate java.lang.VirtualMachineError thrown from assertion` {
      val col = colFun(Set(1, 2, 3))
      intercept[VirtualMachineError] {
        forEvery(col) { (e, _) => throw new VirtualMachineError() {}}
      }
    }
  }

  object `exists/1 ` {
    def colFun[A](s: Set[A]): TableFor1[A] = {

      val table = Table(("column1"), s.toSeq: _*)
      table
    }
    def `should pass when one element passed` {
      val col = colFun(Set(1, 2, 3))
      exists(col) { e => e should be (2) }
    }

    def `should pass when more than one element passed` {
      val col = colFun(Set(1, 2, 3))
      exists(col) { e => e should be < 3 }
    }

    def `should evaluate all elements, and allow test to be canceled after one element passed` {
      val col = colFun(Set(1, 2, 3))
      intercept[TestCanceledException] {
        exists(col) { e =>
          if (e == 3) cancel("You got to three")
          e should be < 3
        }
      }
    }


    def `should throw TestFailedException with correct stack depth and message when none of the elements passed` {
      val col = colFun(Set(1, 2, 3))
      val e = intercept[exceptions.TestFailedException] {
        exists(col) { e =>
          e should be > 5
        }
      }
      e.failedCodeFileName should be (Some("NonGeneratedTableDrivenPropertyChecksSpec.scala"))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
      val itr = col.toIterator
      val first = itr.next
      val second = itr.next
      val third = itr.next; println("Message: " + e.message.value)
      e.message.value should be(s"exists failed, because: \n" +
        s"  org.scalatest.exceptions.TableDrivenPropertyCheckFailedException: TestFailedException was thrown during property evaluation. (NonGeneratedTableDrivenPropertyChecksSpec.scala:${thisLineNumber - 11})\n" +
        s"    Message: 1 was not greater than 5\n" +
        s"    Location: (NonGeneratedTableDrivenPropertyChecksSpec.scala:${thisLineNumber - 12})\n" +
        s"    Occurred at table row 0 (zero based, not counting headings), which had values (\n" +
        s"    column1 = 1  ), \n" +
        s"  org.scalatest.exceptions.TableDrivenPropertyCheckFailedException: TestFailedException was thrown during property evaluation. (NonGeneratedTableDrivenPropertyChecksSpec.scala:${thisLineNumber - 16})\n" +
        s"    Message: 2 was not greater than 5\n" +
        s"    Location: (NonGeneratedTableDrivenPropertyChecksSpec.scala:${thisLineNumber - 17})\n" +
        s"    Occurred at table row 1 (zero based, not counting headings), which had values (\n" +
        s"    column1 = 2  ), \n" +
        s"  org.scalatest.exceptions.TableDrivenPropertyCheckFailedException: TestFailedException was thrown during property evaluation. (NonGeneratedTableDrivenPropertyChecksSpec.scala:${thisLineNumber - 21})\n" +
        s"    Message: 3 was not greater than 5\n" +
        s"    Location: (NonGeneratedTableDrivenPropertyChecksSpec.scala:${thisLineNumber - 22})\n" +
        s"    Occurred at table row 2 (zero based, not counting headings), which had values (\n" +
        s"    column1 = 3  )"
      )
      e.getCause should not be (null)
    }

    def `should pass when all of the elements passed` {
      val col = colFun(Set(1, 2, 3))
      exists(col) { e => e should be < 5 }
    }

    def `should fail in empty case when all of the elements failed` {
      val col = colFun(Set[Int]())
      val e = intercept[exceptions.TestFailedException] {
        exists(col) { e => fail("<there are no elements>") }
      }
    }

    def `should propagate not DiscardedEvaluationException thrown from assertion` {
      val col = colFun(Set(1, 2, 3))
      val e = intercept[exceptions.TestFailedException] {
        exists(col) { _ => throw new DiscardedEvaluationException}
      }
    }

    def `should propagate TestPendingException thrown from assertion` {
      val col = colFun(Set(1, 2, 3))
      intercept[exceptions.TestPendingException] {
        exists(col) { e => pending }
      }
    }

    def `should propagate TestCanceledException thrown from assertion` {
      val col = colFun(Set(1, 2, 3))
      intercept[exceptions.TestCanceledException] {
        exists(col) { e => cancel }
      }
    }

    def `should propagate java.lang.annotation.AnnotationFormatError thrown from assertion` {
      val col = colFun(Set(1, 2, 3))
      intercept[AnnotationFormatError] {
        exists(col) { e => throw new AnnotationFormatError("test") }
      }
    }

    def `should propagate java.nio.charset.CoderMalfunctionError thrown from assertion` {
      val col = colFun(Set(1, 2, 3))
      intercept[CoderMalfunctionError] {
        exists(col) { e => throw new CoderMalfunctionError(new RuntimeException("test")) }
      }
    }

    def `should propagate javax.xml.parsers.FactoryConfigurationError thrown from assertion` {
      val col = colFun(Set(1, 2, 3))
      intercept[FactoryConfigurationError] {
        exists(col) { e => throw new FactoryConfigurationError() }
      }
    }

    def `should propagate java.lang.LinkageError thrown from assertion` {
      val col = colFun(Set(1, 2, 3))
      intercept[LinkageError] {
        exists(col) { e => throw new LinkageError() }
      }
    }

    def `should propagate java.lang.ThreadDeath thrown from assertion` {
      val col = colFun(Set(1, 2, 3))
      intercept[ThreadDeath] {
        exists(col) { e => throw new ThreadDeath() }
      }
    }

    def `should propagate javax.xml.transform.TransformerFactoryConfigurationError thrown from assertion` {
      val col = colFun(Set(1, 2, 3))
      intercept[TransformerFactoryConfigurationError] {
        exists(col) { e => throw new TransformerFactoryConfigurationError() }
      }
    }

    def `should propagate java.lang.VirtualMachineError thrown from assertion` {
      val col = colFun(Set(1, 2, 3))
      intercept[VirtualMachineError] {
        exists(col) { e => throw new VirtualMachineError() {} }
      }
    }
  }

  object `exists/2 ` {
    def colFun[A](s: Set[A]): TableFor2[A, A] = {
      val table = Table(("column1", "column2"), s.map(x => (x, x)).toSeq: _*)
      table
    }

    def `should pass when one element passed` {
      val col = colFun(Set(1, 2, 3))
      exists(col) { (e, _) => e should be (2) }
    }

    def `should pass when more than one element passed` {
      val col = colFun(Set(1, 2, 3))
      exists(col) { (e, _) => e should be < 3 }
    }

    def `should throw TestFailedException with correct stack depth and message when none of the elements passed` {
      val col = colFun(Set(1, 2, 3))
      val e = intercept[exceptions.TestFailedException] {
        exists(col) { (e, _) =>
          e should be > 5
        }
      }
      e.failedCodeFileName should be (Some("NonGeneratedTableDrivenPropertyChecksSpec.scala"))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
      val itr = col.toIterator
      val first = itr.next
      val second = itr.next
      val third = itr.next; println("Message: " + e.message.value)
      e.message.value should be(s"exists failed, because: \n" +
        s"  org.scalatest.exceptions.TableDrivenPropertyCheckFailedException: TestFailedException was thrown during property evaluation. (NonGeneratedTableDrivenPropertyChecksSpec.scala:${thisLineNumber - 11})\n" +
        s"    Message: 1 was not greater than 5\n" +
        s"    Location: (NonGeneratedTableDrivenPropertyChecksSpec.scala:${thisLineNumber - 12})\n" +
        s"    Occurred at table row 0 (zero based, not counting headings), which had values (\n" +
        s"    column1 = 1\n" +
        s"    column2 = 1  ), \n" +
        s"  org.scalatest.exceptions.TableDrivenPropertyCheckFailedException: TestFailedException was thrown during property evaluation. (NonGeneratedTableDrivenPropertyChecksSpec.scala:${thisLineNumber - 17})\n" +
        s"    Message: 2 was not greater than 5\n" +
        s"    Location: (NonGeneratedTableDrivenPropertyChecksSpec.scala:${thisLineNumber - 18})\n" +
        s"    Occurred at table row 1 (zero based, not counting headings), which had values (\n" +
        s"    column1 = 2\n" +
        s"    column2 = 2  ), \n" +
        s"  org.scalatest.exceptions.TableDrivenPropertyCheckFailedException: TestFailedException was thrown during property evaluation. (NonGeneratedTableDrivenPropertyChecksSpec.scala:${thisLineNumber - 23})\n" +
        s"    Message: 3 was not greater than 5\n" +
        s"    Location: (NonGeneratedTableDrivenPropertyChecksSpec.scala:${thisLineNumber - 24})\n" +
        s"    Occurred at table row 2 (zero based, not counting headings), which had values (\n" +
        s"    column1 = 3\n" +
        s"    column2 = 3  )"
      )
      e.getCause should not be (null)
    }

    def `should pass when all of the elements passed` {
      val col = colFun(Set(1, 2, 3))
      exists(col) { (e, _) => e should be < 5 }
    }

    def `should fail in empty case when all of the elements failed` {
      val col = colFun(Set[Int]())
      val e = intercept[exceptions.TestFailedException] {
        exists(col) { (e, _) => fail("<there are no elements>") }
      }
    }

    def `should propagate not DiscardedEvaluationException thrown from assertion` {
      val col = colFun(Set(1, 2, 3))
      val e = intercept[exceptions.TestFailedException] {
        exists(col) { (_, _) => throw new DiscardedEvaluationException}
      }
    }

    def `should propagate TestPendingException thrown from assertion` {
      val col = colFun(Set(1, 2, 3))
      intercept[exceptions.TestPendingException] {
        exists(col) { (_, _) => pending }
      }
    }

    def `should propagate TestCanceledException thrown from assertion` {
      val col = colFun(Set(1, 2, 3))
      intercept[exceptions.TestCanceledException] {
        exists(col) { (_, _) => cancel }
      }
    }

    def `should propagate java.lang.annotation.AnnotationFormatError thrown from assertion` {
      val col = colFun(Set(1, 2, 3))
      intercept[AnnotationFormatError] {
        exists(col) { (_, _) => throw new AnnotationFormatError("test") }
      }
    }

    def `should propagate java.nio.charset.CoderMalfunctionError thrown from assertion` {
      val col = colFun(Set(1, 2, 3))
      intercept[CoderMalfunctionError] {
        exists(col) { (_, _) => throw new CoderMalfunctionError(new RuntimeException("test")) }
      }
    }

    def `should propagate javax.xml.parsers.FactoryConfigurationError thrown from assertion` {
      val col = colFun(Set(1, 2, 3))
      intercept[FactoryConfigurationError] {
        exists(col) { (_, _) => throw new FactoryConfigurationError() }
      }
    }

    def `should propagate java.lang.LinkageError thrown from assertion` {
      val col = colFun(Set(1, 2, 3))
      intercept[LinkageError] {
        exists(col) { (_, _) => throw new LinkageError() }
      }
    }

    def `should propagate java.lang.ThreadDeath thrown from assertion` {
      val col = colFun(Set(1, 2, 3))
      intercept[ThreadDeath] {
        exists(col) { (_, _) => throw new ThreadDeath() }
      }
    }

    def `should propagate javax.xml.transform.TransformerFactoryConfigurationError thrown from assertion` {
      val col = colFun(Set(1, 2, 3))
      intercept[TransformerFactoryConfigurationError] {
        exists(col) { (_, _) => throw new TransformerFactoryConfigurationError() }
      }
    }

    def `should propagate java.lang.VirtualMachineError thrown from assertion` {
      val col = colFun(Set(1, 2, 3))
      intercept[VirtualMachineError] {
        exists(col) { (_, _) => throw new VirtualMachineError() {} }
      }
    }
  }

}

  /*

    describe("forEvery") {
      it("passes with one passing") {
        val example = Table(
          ("passes", "message"),
          (true,     "passes")
        )
        forEvery(example) { (shouldPass, message) =>
          assert(shouldPass, message)
        }
      }

      it("passes with multiple passing") {
        val example = Table(
          ("passes", "message"),
          (true,     "passes"),
          (true,     "passes 2")
        )
        forEvery(example) { (shouldPass, message) =>
          assert(shouldPass, message)
        }
      }

      it("fails with one passing and some failing") {
        pendingUntilFixed {
          val example = Table(
            ("passes", "message"),
            (true, "passes"),
            (false, "failure 1"),
            (false, "failure 2")
          )
          forEvery(example) { (shouldPass, message) =>
            assert(shouldPass, message)
          }
        }
      }

      it("fails when no passing and one failing") {
        pendingUntilFixed {
          val example = Table(
            ("passes", "message"),
            (false, "failure 1")
          )
          forEvery(example) { (shouldPass, message) =>
            assert(shouldPass, message)
          }
        }
      }

      it("fails when no passing and some failing") {
        pendingUntilFixed {
          val example = Table(
            ("passes", "message"),
            (false, "failure 1"),
            (false, "failure 2"),
            (false, "failure 3")
          )
          forEvery(example) { (shouldPass, message) =>
            assert(shouldPass, message)
          }
        }
      }
    }
}
*/
/*
class InspectorExampleSpec extends Spec with Matchers with Inspectors {
  object `Inspector examples ` {
    val list = Seq(1, 2, 3)
    def `failing list for forAll` = {
      forAll(list) { x =>
        x should be < (2)
      }
    }
    def `failing list for forEvery` = {
      forEvery(list) { x =>
        x should be < (2)
      }
    }
  }
}

class NonGeneratedTableDrivenPropertyChecksSpec extends Spec with Matchers with NonGeneratedTableDrivenPropertyChecks {

  object `examples for failing ` {
    def `failing table for 1 forAll` {
      val examples = Table(("a"),
        (1))
      forAll(examples) { a =>
        a should be (2)
      }
    }

    def `failing table for 2 forAll` {
      val examples = Table(("a", "b"),
        (1, 2))

      forAll(examples) { case (a, b) =>
        assert(a === 42)
      }
    }
  }
}
*/
