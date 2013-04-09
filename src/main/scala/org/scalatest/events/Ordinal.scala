/*
 * Copyright 2001-2008 Artima, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.scalatest.events

import org.scalatest._

import java.util.Arrays

/**
 * Class used to specify a sequential order for events reported during a test run, so they
 * can be arranged in that order in a report even if the events were fired in some other order
 * during concurrent or distributed execution.
 *
 * <p>
 * An <code>Ordinal</code> is an immutable object holding a <em>run stamp</em> and a sequence
 * of <em>stamps</em>.
 * The run stamp is an integer that identifies a particular run. All events
 * reported during the same run should share the same run stamp. By contrast, each
 * event reported during a particular run should have a different stamps sequence.
 * One use case for the run stamp is that the initial run from ScalaTest's GUI
 * will have run stamp 0. Subsequent reruns will have run stamps 1,
 * 2, 3, <em>etc.</em>, so that reports in the GUI can simply be sorted in "ordinal" order. Another
 * use case is a set of servers used to run multiple tests simultaneously in a distributed
 * fashion. The run stamp can be used to identify the run to which an event belongs.
 * </p>
 *
 * <p>
 * The stamps sequence is designed to allow a sequential order of events to be specified during
 * concurrent execution of ScalaTest suites. ScalaTest's model for concurrent execution is that
 * the suites that make up a run may be executed concurrently, but the tests within a single suite
 * will be executed sequentially. In addition to tests, suites may contain nested suites. The default implementation
 * of <code>execute</code> in class <code>Suite</code> will first invoke <code>runNestedSuites</code> and
 * then <code>runTests</code>. If no <code>Distributor</code> is passed to <code>execute</code>, the
 * <code>runNestedSuites</code> method will execute the nested suites sequentially via the same thread
 * that invoked <code>runNestedSuites</code>. As a result, suites will by default executed in depth first order
 * when executed sequentially. If a <code>Distributor</code> is passed to <code>execute</code>, the
 * <code>runNestedSuites</code> method will simply put its nested suites into the <code>Distributor</code>
 * and return. Some other threads or processes must then execute those nested suites. Given the default
 * implementations of <code>execute</code> and <code>runNestedSuites</code> described here, the <code>Ordinal</code>
 * will allow the events from a concurrent run to be sorted in the same depth-first order that the events
 * from a corresponding sequential run would arrive.
 * </p>
 *
 * <p>
 * Each event reported during a run should be given a unique <code>Ordinal</code>. An <code>Ordinal</code> is required
 * by all <code>Event</code> subclasses, instances of which are used to send information to the <code>report</code>
 * function passed to a <code>Suite</code>'s <code>execute</code> method. The first <code>Ordinal</code> for a run
 * can be produced by passing a run stamp to <code>Ordinal</code>'s lone public constructor:
 * </p>
 * 
 * <pre class="stHighlight">
 * val firstOrdinal = new Ordinal(99)
 * </pre>
 *
 * <p>
 * The run stamp can be any integer. The <code>Ordinal</code> created in this way can be passed along with the first
 * reported event of the run, such as a <code>RunStarting</code> event. Thereafter, new <code>Ordinal</code>s for the same run
 * can be obtained by calling either <code>next</code> or <code>nextNewOldPair</code> on the previously obtained <code>Ordinal</code>.
 * In other words, given an <code>Ordinal</code>, you can obtain the next <code>Ordinal</code> by invoking one of these two
 * "next" methods on the <code>Ordinal</code> you have in hand. Before executing a new <code>Suite</code>, the <code>nextNewOldPair</code>
 * method should be invoked. This will return two new <code>Ordinal</code>s, one for the new <code>Suite</code> about to be executed, and
 * one for the currently executing entity (either a <code>Suite</code> or some sort of test runner). At any other time, the next <code>Ordinal</code>
 * can be obtained by simply invoking <code>next</code> on the current <code>Ordinal</code>.
 * </p>
 *
 * <p>
 * You can convert an <code>Ordinal</code> to a <code>List</code> by invoking <code>toList</code> on it. The resulting <code>List</code> will contain
 * the run stamp as its first element, and the contents of its stamps sequence as the subsequent elements. The stamps
 * sequence will initially be composed of a single element with the value 0. Thus, <code>toList</code> invoked on the <code>firstOrdinal</code> shown above will 
 * result in:
 * </p>
 * 
 * <pre class="stHighlight">
 * firstOrdinal.toList // results in: List(99, 0)
 * </pre>
 *
 * <p>
 * Each time <code>next</code> is invoked, the rightmost integer returned by <code>toList</code> will increment: 
 * </p>
 * 
 * <pre class="stHighlight">
 * val secondOrdinal = firstOrdinal.next
 * secondOrdinal.toList // results in: List(99, 1)
 * 
 * val thirdOrdinal = secondOrdinal.next
 * thirdOrdinal.toList  // result is : List(99, 2)
 * </pre>
 *
 * <p>
 * When <code>nextNewOldPair</code> is invoked the result will be a tuple whose first element is the first <code>Ordinal</code> for
 * the new <code>Suite</code> about to be executed (for example, a nested <code>Suite</code> of the currently executing <code>Suite</code>). The
 * second element is the next <code>Ordinal</code> for the currently executing <code>Suite</code> or other entity:
 * </p>
 *
 * <pre class="stHighlight">
 * val (nextForNewSuite, nextForThisRunner) = thirdOrdinal.nextNewOldPair
 * nextForNewSuite.toList   // results in: (99, 2, 0)
 * nextForThisRunner.toList // results in: (99, 3)
 * </pre>
 *
 * <p>
 * The <code>toList</code> method of the <code>Ordinal</code> for the new suite starts with the same sequence of elements as the <code>Ordinal</code> from which it was
 * created, but has one more element, a 0, appended at the end. Subsequent invocations of <code>next</code> on this series of <code>Ordinal</code>s will
 * increment that last element:
 * </p>
 *
 * <pre class="stHighlight">
 * val newSuiteOrdinal2 = nextForNewSuite.next
 * newSuiteOrdinal2.toList // results in: List(99, 2, 1)
 * 
 * val newSuiteOrdinal3 = newSuiteOrdinal2.next
 * newSuiteOrdinal3.toList  // result is : List(99, 2, 2)
 * </pre>
 *
 * <p>
 * This behavior allows events fired by <code>Suite</code> running concurrently to be reordered in a pre-determined sequence after all the events
 * have been reported. The ordering of two <code>Ordinal</code>s can be determined by first comparing the first element of the <code>List</code>s obtained
 * by invoking <code>toList</code> on both <code>Ordinal</code>s. These values represent the <code>runStamp</code>. If one run stamp is a lower number than
 * the other, that <code>Ordinal</code> comes first. For example, an <code>Ordinal</code> with a run stamp of 98 is ordered before an <code>Ordinal</code> with
 * a run stamp of 99. If the run stamps are equal, the next number in the list is inspected. As with the run stamps, an  <code>Ordinal</code> with a lower
 * number is ordered before an <code>Ordinal</code> with a higher number. If two corresponding elements are equal, the next pair of elements will be inspected.
 * This will continue no down the length of the <code>List</code>s until a position is found where the element values are not equal, or the end of one or both of
 * the <code>List</code>s are reached. If the two <code>List</code>s are identical all the way to the end, and both <code>List</code>s have the same lengths, 
 * then the <code>Ordinal</code>s are equal. (Equal <code>Ordinal</code>s will not happen if correctly used by creating a new <code>Ordinal</code> for
 * each fired event and each new <code>Suite</code>.). If the two <code>List</code>s are identical all the way to the end of one, but the other <code>List</code>
 * is longer (has more elements), then the shorter list is ordered before the longer one.
 * </p>
 *
 * <p>
 * As an example, here are some <code>Ordinal</code> <code>List</code> forms in order:
 * </p>
 *
 * <pre>
 * List(99, 0)
 * List(99, 1)
 * List(99, 2)
 * List(99, 2, 0)
 * List(99, 2, 1)
 * List(99, 2, 2)
 * List(99, 2, 2, 0)
 * List(99, 2, 2, 1)
 * List(99, 2, 2, 2)
 * List(99, 2, 3)
 * List(99, 2, 4)
 * List(99, 2, 4, 0)
 * List(99, 2, 4, 1)
 * List(99, 2, 4, 2)
 * List(99, 3)
 * List(99, 4)
 * List(99, 4, 0)
 * List(99, 4, 1)
 * List(99, 5)
 * </pre>
 *
 * @author Bill Venners
 */
final class Ordinal private (val runStamp: Int, private val stamps: Array[Int]) extends Ordered[Ordinal] with java.io.Serializable {

  /**
   * Construct a the first <code>Ordinal</code> for a run.
   *
   * @param runStamp a number that identifies a particular run
   */
  def this(runStamp: Int) = this(runStamp, Array(0))

  /**
   * Construct the next <code>Ordinal</code> for the current suite or other entity, such as a runner.
   */
  def next: Ordinal = {
    val newArray = new Array[Int](stamps.length) // Can't seem to clone
    val zipped = stamps.zipWithIndex
    for ((num, idx) <- zipped)
      newArray(idx) = num
    newArray(stamps.length - 1) += 1
    new Ordinal(runStamp, newArray)
  }

  /**
   * Construct two new <code>Ordinal</code>s, one for a new <code>Suite</code> about to be executed and
   * one for the current <code>Suite</code> or other entity, such as a runner. The <code>Ordinal</code>
   * for the new <code>Suite</code> is the first (<code>_1</code>) element in the tuple:
   *
   * <pre class="stHighlight">
   * val (nextOrdinalForNewSuite, nextOrdinalForThisSuite) currentOrdinal.nextNewOldPair
   * </pre>
   *
   * <p>
   * The reason the next <code>Ordinal</code> for the new <code>Suite</code> is first is because it will
   * be ordered <em>before</em> the next <code>Ordinal</code> for the current <code>Suite</code> (or other
   * entity such as a runner). In fact, any event reported within the context of the new <code>Suite</code> or
   * its nested <code>Suite</code>s will be ordered before the next <code>Ordinal</code> for the current <code>Suite</code>.
   * </p>
   *
   * @return a tuple whose first element is the first <code>Ordinal</code> for the new <code>Suite</code> and whose
   *          second element is the next <code>Ordinal</code> for the current <code>Suite</code> or other entity, such
   *          as a runner.
   */
  def nextNewOldPair: (Ordinal, Ordinal) = {
    val newArrayForNewSuite = new Array[Int](stamps.length + 1)
    val newArrayForOldSuite = new Array[Int](stamps.length)
    val zipped = stamps.zipWithIndex
    for ((num, idx) <- zipped) {
      newArrayForNewSuite(idx) = num
      newArrayForOldSuite(idx) = num
    }
    newArrayForOldSuite(stamps.length - 1) += 1
    (new Ordinal(runStamp, newArrayForNewSuite), new Ordinal(runStamp, newArrayForOldSuite))
  }

  /**
   * Returns a <code>List[Int]</code> representation of this <code>Ordinal</code>. A set of <code>Ordinal</code>s will be ordered
   * in the same order as the set of <code>List[Int]</code>s that are returned by invoking this method on each of the <code>Ordinal</code>s.
   * The first element of the returned <code>List[Int]</code> is the <code>runStamp</code>.
   *
   * @return a <code>List[Int]</code> representation of this <code>Ordinal</code>.
   */
  def toList: List[Int] = runStamp :: stamps.toList

  /**
   * Compares this <code>Ordinal</code> with the passed <code>Ordinal</code> for order. If this object is "less than" (ordered before)
   * the passed object, <code>compare</code> will return a negative integer. If this class is "greater than" (ordered after)
   * the passed object, <code>compare</code> will return a positive integer. Otherwise, this <code>Ordinal</code> is equal to
   * the passed object, and <code>compare</code> will return 0.
   * 
   * @return a negative integer, 0, or positive integer indicating this <code>Ordinal</code> is less than, equal to, or greater than the passed <code>Ordinal</code>.
   */
  def compare(that: Ordinal) = {
    val runStampDiff = this.runStamp - that.runStamp
    if (runStampDiff == 0) {
      val shorterLength =
        if (this.stamps.length < that.stamps.length)
          this.stamps.length
        else
          that.stamps.length
      var i = 0
      var diff = 0
      while (diff == 0 && i < shorterLength) {
        diff = this.stamps(i) - that.stamps(i)
        i += 1
      }
      // If they were equal all the way to the shorterLength, the longest array
      // one is the greater ordinal. This is because the newSuite stuff happens
      // before the next thing that happens in the old suite.
      if (diff != 0) diff
      else this.stamps.length - that.stamps.length
    }
    else runStampDiff
  }

  /**
   * Indicates whether the passed object is equal to this one.
   *
   * @param the object with which to compare this one for equality
   * @return true if the passed object is equal to this one
   */
  override def equals(other: Any): Boolean =
    other match {
      case that: Ordinal =>
        runStamp == that.runStamp &&
        (stamps.deep equals that.stamps.deep)
      case _ => false
    }

  /**
   * Returns a hash code value for this object.
   *
   * @return a hash code for this object
   */
  override def hashCode: Int =
    41 * (
      41 + runStamp
    ) + Arrays.hashCode(stamps)

  /**
   * Returns a string that includes the integers returned by <code>toList</code>.
   */
  override def toString: String = toList.mkString("Ordinal(", ", ", ")")
}
