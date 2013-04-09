package org.scalatest

import scala.collection.GenTraversable
import scala.annotation.tailrec
import org.scalatest.exceptions.StackDepthExceptionHelper.getStackDepthFun
import scala.collection.GenSeq
import Suite.indentLines

/**
 * Provides nestable <em>inspector methods</em> (or just <em>inspectors</em>) that enable assertions to be made about collections.
 *
 * <p>
 * For example, the <code>forAll</code> method enables you to state that something should be true about all elements of a collection, such
 * as that all elements should be positive:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; import org.scalatest._
 * import org.scalatest._
 *
 * scala&gt; import Assertions._
 * import Assertions._
 *
 * scala&gt; import Inspectors._
 * import Inspectors._
 *
 * scala&gt; val xs = List(1, 2, 3, 4, 5)
 * xs: List[Int] = List(1, 2, 3, 4, 5)
 *
 * scala&gt; forAll (xs) { x =&gt; assert(x &gt; 0) }
 * </pre>
 *
 * <p>
 * Or, with matchers:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; import Matchers._
 * import Matchers._
 *
 * scala&gt; forAll (xs) { x =&gt; x should be &gt; 0 }
 * </pre>
 *
 * <p>
 * To make assertions about nested collections, you can nest the inspector method invocations.
 * For example, given the following list of lists of <code>Int</code>:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; val yss =
 *      |   List(
 *      |     List(1, 2, 3),
 *      |     List(1, 2, 3),
 *      |     List(1, 2, 3)
 *      |   )
 * yss: List[List[Int]] = List(List(1, 2, 3), List(1, 2, 3), List(1, 2, 3))
 * </pre>
 *
 * <p>
 * You can assert that all <code>Int</code> elements in all nested lists are positive by nesting two <code>forAll</code> method invocations, like this:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; forAll (yss) { ys =&gt;
 *      |   forAll (ys) { y =&gt; y should be &gt; 0 }
 *      | }
 * </pre>
 *
 * <p>
 * The full list of inspector methods are:
 * </p>
 *
 * <ul>
 * <li><code>forAll</code> - succeeds if the assertion holds true for every element</li>
 * <li><code>forAtLeast</code> - succeeds if the assertion holds true for at least the specified number of elements</li>
 * <li><code>forAtMost</code> - succeeds if the assertion holds true for at most the specified number of elements</li>
 * <li><code>forBetween</code> - succeeds if the assertion holds true for between the specified minimum and maximum number of elements, inclusive</li>
 * <li><code>forEvery</code> - same as <code>forAll</code>, but lists all failing elements if it fails (whereas <code>forAll</code> just reports the first failing element)</li>
 * <li><code>forExactly</code> - succeeds if the assertion holds true for exactly the specified number of elements</li>
 * </ul>
 *
 * <p>
 * The error messages produced by inspector methods are designed to make sense no matter how deeply you nest the method invocations. 
 * Here's an example of a nested inspection that fails and the resulting error message:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; forAll (yss) { ys =&gt;
 *      |   forAll (ys) { y =&gt; y should be &lt; 2 }
 *      | }
 * org.scalatest.exceptions.TestFailedException: forAll failed, because: 
 *   at index 0, forAll failed, because: 
 *     at index 1, 2 was not less than 2 (&lt;console&gt;:20) 
 *   in List(1, 2, 3) (&lt;console&gt;:20) 
 * in List(List(1, 2, 3), List(1, 2, 3), List(1, 2, 3))
 *      at org.scalatest.InspectorsHelper$.doForAll(Inspectors.scala:146)
 *      ...
 * </pre>
 *
 * <p>
 * One way the error message is designed to help you understand the error is by using indentation that mimics the indentation of the
 * source code (optimistically assuming the source will be nicely indented). The error message above indicates the outer <code>forAll</code> failed
 * because its initial <code>List</code> (<em>i.e.</em>, at index 0) failed
 * the assertion, which was that all elements of that initial <code>List[Int]</code> at index 0 should be less than 2. This assertion failed because index 1 of
 * that inner list contained the value 2, which was indeed &ldquo;not less than 2.&rdquo; The error message for the inner list is an indented line inside the error message
 * for the outer list. The actual contents of each list are displayed at the end in inspector error messages, also indented appropriately. The actual contents
 * are placed at the end so that for very large collections, the contents will not drown out and make it difficult to find the messages that describe
 * actual causes of the failure.
 * </p>
 *
 * <p>
 * The <code>forAll</code> and <code>forEvery</code> methods are similar in that both succeed only if the assertion holds for all elements of the collection.
 * They differ in that <code>forAll</code> will only report the first element encountered that failed the assertion, but <code>forEvery</code> will report <em>all</em>
 * elements that fail the assertion. The tradeoff is that while <code>forEvery</code> gives more information, it may take longer to run because it must inspect every element
 * of the collection. The <code>forAll</code> method can simply stop inspecting once it encounters the first failing element. Here's an example that
 * shows the difference in the <code>forAll</code> and <code>forEvery</code> error messages:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; forAll (xs) { x =&gt; x should be &lt; 3 }
 * org.scalatest.exceptions.TestFailedException: forAll failed, because: 
 *   at index 2, 3 was not less than 3 (&lt;console&gt;:18) 
 * in List(1, 2, 3, 4, 5)
 *      at org.scalatest.InspectorsHelper$.doForAll(Inspectors.scala:146)
 *      ...
 *
 * scala&gt; forEvery (xs) { x =&gt; x should be &lt; 3 }
 * org.scalatest.exceptions.TestFailedException: forEvery failed, because: 
 *   at index 2, 3 was not less than 3 (&lt;console&gt;:18), 
 *   at index 3, 4 was not less than 3 (&lt;console&gt;:18), 
 *   at index 4, 5 was not less than 3 (&lt;console&gt;:18) 
 * in List(1, 2, 3, 4, 5)
 *      at org.scalatest.InspectorsHelper$.doForEvery(Inspectors.scala:226)
 *      ...
 * </pre>
 *
 * <p>
 * Note that if you're using matchers, you can alternatively use <em>inspector shorthands</em> for writing non-nested
 * inspections. Here's an example:
 * </p>
 * 
 * <pre>
 * 
 * scala&gt; all (xs) should be &gt; 3
 * org.scalatest.exceptions.TestFailedException: 'all' inspection failed, because: 
 *   at index 0, 1 was not greater than 3 
 * in List(1, 2, 3, 4, 5)
 *      at org.scalatest.InspectorsHelper$.doForAll(Inspectors.scala:146)
 * </pre>
 */
trait Inspectors {

  import InspectorsHelper._

  def forAll[T](xs: GenTraversable[T])(fun: T => Unit) {
    doForAll(xs, "forAllFailed", "Inspectors.scala", "forAll", 0)(fun)
  }

  def forAtLeast[T](min: Int, xs: GenTraversable[T])(fun: T => Unit) {
    doForAtLeast(min, xs, "forAtLeastFailed", "Inspectors.scala", "forAtLeast", 0)(fun)
  }

  private def shouldIncludeIndex[T, R](xs: GenTraversable[T]) = xs.isInstanceOf[GenSeq[T]]

  private def createElementsMessage[T](elements: IndexedSeq[(Int, T)], includeIndex: Boolean): String = elements.map { case (index, element) => 
    if (includeIndex) 
      Resources("forAssertionsMessageWithIndex", index.toString, element.toString) 
    else 
      Resources("forAssertionsMessageWithoutIndex", element.toString) 
  }.mkString(", ")

  def forAtMost[T](max: Int, xs: GenTraversable[T])(fun: T => Unit) {
    doForAtMost(max, xs, "forAtMostFailed", "Inspectors.scala", "forAtMost", 0)(fun)
  }

  def forExactly[T](succeededCount: Int, xs: GenTraversable[T])(fun: T => Unit) {
    doForExactly(succeededCount, xs, "forExactlyFailed", "Inspectors.scala", "forExactly", 0)(fun)
  }
  
  private[scalatest] def forNo[T](xs: GenTraversable[T])(fun: T => Unit) {
    doForNo(xs, "forNoFailed", "Inspectors.scala", "forNo", 0)(fun)
  }
  
  def forBetween[T](from: Int, upTo: Int, xs: GenTraversable[T])(fun: T => Unit) {
    doForBetween(from, upTo, xs, "forBetweenFailed", "Inspectors.scala", "forBetween", 0)(fun)
  }
  
  def forEvery[T](xs: GenTraversable[T])(fun: T => Unit) {
    doForEvery(xs, "forEveryFailed", "Inspectors.scala", "forEvery", 0)(fun)
  }
}

object Inspectors extends Inspectors

private[scalatest] object InspectorsHelper {
  
  def indentErrorMessages(messages: IndexedSeq[String]) = indentLines(1, messages)
  
  def getResourceNamePrefix(xs: GenTraversable[_]): String = 
    xs match {
      case _: collection.GenMap[_, _] => "forAssertionsGenMapMessage"
      case _ => "forAssertionsGenTraversableMessage"
    }
  
  def shouldPropagate(throwable: Throwable): Boolean = 
    throwable match {
      case _: exceptions.TestPendingException |
           _: exceptions.TestCanceledException => true
      case _ if Suite.anExceptionThatShouldCauseAnAbort(throwable) => true
      case _ => false
    }
  
  def createMessage(messageKey: String, t: Throwable, resourceNamePrefix: String): String = 
    t match {
      case sde: exceptions.StackDepthException => 
        sde.failedCodeFileNameAndLineNumberString match {
          case Some(failedCodeFileNameAndLineNumber) => 
            Resources(resourceNamePrefix + "WithStackDepth", messageKey, sde.getMessage, failedCodeFileNameAndLineNumber)
          case None => 
            Resources(resourceNamePrefix + "WithoutStackDepth", messageKey, sde.getMessage)
        }
        
    }
  
  def elementLabel(count: Int): String = 
    if (count > 1) Resources("forAssertionsElements", count.toString) else Resources("forAssertionsElement", count.toString)
  
  case class ForResult[T](passedCount: Int = 0, messageAcc: IndexedSeq[String] = IndexedSeq.empty, 
                                 passedElements: IndexedSeq[(Int, T)] = IndexedSeq.empty, failedElements: IndexedSeq[(Int, T, Throwable)] = IndexedSeq.empty)
  
  @tailrec
  def runFor[T](itr: Iterator[T], resourceNamePrefix: String, index:Int, result: ForResult[T], fun: T => Unit, stopFun: ForResult[_] => Boolean): ForResult[T] = {
    if (itr.hasNext) {
      val head = itr.next
      val newResult = 
        try {
          fun(head)
          result.copy(passedCount = result.passedCount + 1, passedElements = result.passedElements :+ (index, head))
        }
        catch {
          case e if !shouldPropagate(e) => 
            val messageKey = head match {
              case tuple: Tuple2[_, _] if resourceNamePrefix == "forAssertionsGenMapMessage" => tuple._1.toString
              case _ => index.toString
            }
            result.copy(messageAcc = result.messageAcc :+ createMessage(messageKey, e, resourceNamePrefix), failedElements = result.failedElements :+ (index, head, e))
        }
      if (stopFun(newResult))
        newResult
      else
        runFor(itr, resourceNamePrefix, index + 1, newResult, fun, stopFun)
    }
    else
      result
  }
  
  def keyOrIndexLabel(xs: GenTraversable[_], passedElements: IndexedSeq[(Int, _)]): String = {
    def makeAndLabel(indexes: IndexedSeq[Int]): String = 
      if (indexes.length > 1)
        indexes.dropRight(1).mkString(", ") + " and " + indexes.last
      else
        indexes.mkString(", ")
      
    val (prefixResourceName, elements) = xs match {
      case map: collection.GenMap[_, _] => 
        val elements = passedElements.map{ case (index, e) => 
          e match {
            case tuple2: Tuple2[_, _] => tuple2._1
            case _ => index
          }
        }
        ("forAssertionsKey", elements)
      case _ => 
        ("forAssertionsIndex", passedElements.map(_._1))
    }
    
    if (elements.length > 1)
      Resources(prefixResourceName + "AndLabel", elements.dropRight(1).mkString(", "), elements.last.toString) 
    else
      Resources(prefixResourceName + "Label", elements.mkString(", "))
  }
  
  def doForAll[T](xs: GenTraversable[T], resourceName: String, sourceFileName: String, methodName: String, stackDepthAdjustment: Int)(fun: T => Unit) {
    val resourceNamePrefix = getResourceNamePrefix(xs)
    val result = 
      runFor(xs.toIterator, resourceNamePrefix, 0, new ForResult[T], fun, _.failedElements.length > 0)
    if (result.failedElements.length > 0) 
      throw new exceptions.TestFailedException(
        sde => Some(Resources(resourceName, indentErrorMessages(result.messageAcc).mkString(", \n"), xs.toString)),
        Some(result.failedElements(0)._3),
        getStackDepthFun(sourceFileName, methodName, stackDepthAdjustment)
      )
  }
  
  def doForAtLeast[T](min: Int, xs: GenTraversable[T], resourceName: String, sourceFileName: String, methodName: String, stackDepthAdjustment: Int)(fun: T => Unit) {
    @tailrec
    def forAtLeastAcc(itr: Iterator[T], includeIndex: Boolean, index: Int, passedCount: Int, messageAcc: IndexedSeq[String]): (Int, IndexedSeq[String]) = {
      if (itr.hasNext) {
        val head = itr.next
        val (newPassedCount, newMessageAcc) = 
          try {
            fun(head)
            (passedCount + 1, messageAcc)
          }
          catch {
            case e if !shouldPropagate(e) => 
              val resourceNamePrefix = getResourceNamePrefix(xs)
              val messageKey = head match {
                case tuple: Tuple2[_, _] if resourceNamePrefix == "forAssertionsGenMapMessage" => tuple._1.toString
                case _ => index.toString
              }
              (passedCount, messageAcc :+ createMessage(messageKey, e, resourceNamePrefix))
          }
        if (newPassedCount < min)
          forAtLeastAcc(itr, includeIndex, index + 1, newPassedCount, newMessageAcc)
        else
          (newPassedCount, newMessageAcc)
      }
      else
        (passedCount, messageAcc)
    }
    
    if (min <= 0)
      throw new IllegalArgumentException(Resources("forAssertionsMoreThanZero", "'min'"))
    
    val (passedCount, messageAcc) = forAtLeastAcc(xs.toIterator, xs.isInstanceOf[Seq[T]], 0, 0, IndexedSeq.empty)
    if (passedCount < min)
      throw new exceptions.TestFailedException(
        sde => 
          Some(
            if (passedCount > 0)
              Resources(resourceName, min.toString, elementLabel(passedCount), indentErrorMessages(messageAcc).mkString(", \n"), xs.toString)
            else
              Resources(resourceName + "NoElement", min.toString, indentErrorMessages(messageAcc).mkString(", \n"), xs.toString)
          ),
        None,
        getStackDepthFun(sourceFileName, methodName, stackDepthAdjustment)
      )
  }
  
  def doForEvery[T](xs: GenTraversable[T], resourceName: String, sourceFileName: String, methodName: String, stackDepthAdjustment: Int)(fun: T => Unit) {
    @tailrec
    def runAndCollectErrorMessage[T](itr: Iterator[T], messageList: IndexedSeq[String], index: Int)(fun: T => Unit): IndexedSeq[String] = {
      if (itr.hasNext) {
        val head = itr.next
        val newMessageList = 
          try {
            fun(head)
            messageList
          }
          catch {
            case e if !shouldPropagate(e) => 
              val resourceNamePrefix = getResourceNamePrefix(xs)
              val messageKey = head match {
                case tuple: Tuple2[_, _] if resourceNamePrefix == "forAssertionsGenMapMessage" => tuple._1.toString
                case _ => index.toString
              }
              messageList :+ createMessage(messageKey, e, resourceNamePrefix)
          }
        
        runAndCollectErrorMessage(itr, newMessageList, index + 1)(fun)
      }
      else
        messageList
    }
    val messageList = runAndCollectErrorMessage(xs.toIterator, IndexedSeq.empty, 0)(fun)
    if (messageList.size > 0)
      throw new exceptions.TestFailedException(
          sde => Some(Resources(resourceName, indentErrorMessages(messageList).mkString(", \n"), xs)),
          None,
          getStackDepthFun(sourceFileName, methodName, stackDepthAdjustment)
        )
  }
  
  def doForExactly[T](succeededCount: Int, xs: GenTraversable[T], resourceName: String, sourceFileName: String, methodName: String, stackDepthAdjustment: Int)(fun: T => Unit) {
    if (succeededCount <= 0)
      throw new IllegalArgumentException(Resources("forAssertionsMoreThanZero", "'succeededCount'"))
    
    val resourceNamePrefix = getResourceNamePrefix(xs)
    val result = 
      runFor(xs.toIterator, resourceNamePrefix, 0, new ForResult[T], fun, _.passedCount > succeededCount)
    if (result.passedCount != succeededCount)
      throw new exceptions.TestFailedException(
        sde => 
          Some(
            if (result.passedCount == 0)
              Resources(resourceName + "NoElement", succeededCount.toString, indentErrorMessages(result.messageAcc).mkString(", \n"), xs.toString)
            else {
              if (result.passedCount < succeededCount)
                Resources(resourceName + "Less", succeededCount.toString, elementLabel(result.passedCount), keyOrIndexLabel(xs, result.passedElements), indentErrorMessages(result.messageAcc).mkString(", \n"), xs.toString)
              else
                Resources(resourceName + "More", succeededCount.toString, elementLabel(result.passedCount), keyOrIndexLabel(xs, result.passedElements), xs.toString)
            }
          ),
        None,
        getStackDepthFun(sourceFileName, methodName, stackDepthAdjustment)
      )
  }

  def doForNo[T](xs: GenTraversable[T], resourceName: String, sourceFileName: String, methodName: String, stackDepthAdjustment: Int)(fun: T => Unit) {
    val resourceNamePrefix = getResourceNamePrefix(xs)
    val result =
      runFor(xs.toIterator, resourceNamePrefix, 0, new ForResult[T], fun, _.passedCount != 0)
    if (result.passedCount != 0)
      throw new exceptions.TestFailedException(
        sde => Some(Resources(resourceName, keyOrIndexLabel(xs, result.passedElements), xs)),
        None,
        getStackDepthFun(sourceFileName, methodName, stackDepthAdjustment)
      )
  }

  def doForBetween[T](from: Int, upTo: Int, xs: GenTraversable[T], resourceName: String, sourceFileName: String, methodName: String, stackDepthAdjustment: Int)(fun: T => Unit) {
    if (from < 0)
      throw new IllegalArgumentException(Resources("forAssertionsMoreThanEqualZero", "'from'"))
    if (upTo <= 0)
      throw new IllegalArgumentException(Resources("forAssertionsMoreThanZero", "'upTo'"))
    if (upTo <= from)
      throw new IllegalArgumentException(Resources("forAssertionsMoreThan", "'upTo'", "'from'"))

    val resourceNamePrefix = getResourceNamePrefix(xs)
    val result =
      runFor(xs.toIterator, resourceNamePrefix, 0, new ForResult[T], fun, _.passedCount > upTo)
    if (result.passedCount < from || result.passedCount > upTo)
      throw new exceptions.TestFailedException(
        sde =>
          Some(
            if (result.passedCount == 0)
              Resources(resourceName + "NoElement", from.toString, upTo.toString, indentErrorMessages(result.messageAcc).mkString(", \n"), xs)
            else {
              if (result.passedCount < from)
                Resources(resourceName + "Less", from.toString, upTo.toString, elementLabel(result.passedCount), keyOrIndexLabel(xs, result.passedElements), indentErrorMessages(result.messageAcc).mkString(", \n"), xs)
              else
                Resources(resourceName + "More", from.toString, upTo.toString, elementLabel(result.passedCount), keyOrIndexLabel(xs, result.passedElements), xs)
            }
          ),
        None,
        getStackDepthFun(sourceFileName, methodName, stackDepthAdjustment)
      )
  }

  def doForAtMost[T](max: Int, xs: GenTraversable[T], resourceName: String, sourceFileName: String, methodName: String, stackDepthAdjustment: Int)(fun: T => Unit) {
    if (max <= 0)
      throw new IllegalArgumentException(Resources("forAssertionsMoreThanZero", "'max'"))

    val resourceNamePrefix = getResourceNamePrefix(xs)
    val result =
      runFor(xs.toIterator, resourceNamePrefix, 0, new ForResult[T], fun, _.passedCount > max)
    if (result.passedCount > max)
      throw new exceptions.TestFailedException(
        sde => Some(Resources(resourceName, max.toString, result.passedCount.toString, keyOrIndexLabel(xs, result.passedElements), xs.toString)),
        None,
        getStackDepthFun(sourceFileName, methodName, stackDepthAdjustment)
      )
  }
}
