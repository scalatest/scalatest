/*
 * Copyright 2001-2013 Artima, Inc.
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
package org.scalatest

import Filter.IgnoreTag
import org.scalactic.Requirements._

/**
 * Filter whose <code>apply</code> method determines which of the passed tests to run and ignore based on tags to include and exclude passed as
 * as class parameters.
 *
 * <p>
 * This class handles the <code>org.scalatest.Ignore</code> tag specially, in that its <code>apply</code> method indicates which
 * tests should be ignored based on whether they are tagged with <code>org.scalatest.Ignore</code>. If
 * <code>"org.scalatest.Ignore"</code> is not passed in the <code>tagsToExclude</code> set, it will be implicitly added. However, if the 
 * <code>tagsToInclude</code> option is defined, and the contained set does not include <code>"org.scalatest.Ignore"</code>, then only those tests
 * that are both tagged with <code>org.scalatest.Ignore</code> and at least one of the tags in the <code>tagsToInclude</code> set
 * will be included in the result of <code>apply</code> and marked as ignored (so long as the test is not also
 * marked with a tag other than <code>org.scalatest.Ignore</code> that is a member of the <code>tagsToExclude</code>
 * set. For example, if <code>SlowAsMolasses</code> is a member of the <code>tagsToInclude</code> set and a
 * test is tagged with both <code>org.scalatest.Ignore</code> and <code>SlowAsMolasses</code>, and
 * <code>SlowAsMolasses</code> appears in the <code>tagsToExclude</code> set, the
 * <code>SlowAsMolasses</code> tag will "overpower" the <code>org.scalatest.Ignore</code> tag, and the
 * test will be filtered out entirely rather than being ignored.
 * </p>
 *
 * @param tagsToInclude an optional <code>Set</code> of <code>String</code> tag names to include (<em>i.e.</em>, not filter out) when filtering tests
 * @param tagsToExclude a <code>Set</code> of <code>String</code> tag names to exclude (<em>i.e.</em>, filter out) when filtering tests
 * @param excludeNestedSuites a <code>Boolean</code> to indicate whether to run nested suites
 * @param dynaTags dynamic tags for the filter
 *
 * @throws NullArgumentException if either <code>tagsToInclude</code> or <code>tagsToExclude</code> are null
 * @throws IllegalArgumentException if <code>tagsToInclude</code> is defined, but contains an empty set
 */
final class Filter private (val tagsToInclude: Option[Set[String]], val tagsToExclude: Set[String], val excludeNestedSuites: Boolean, val dynaTags: DynaTags) extends Serializable {

  requireNonNull(tagsToInclude, tagsToExclude, dynaTags)

  tagsToInclude match {
    case Some(tagsToInclude) =>
      if (tagsToInclude.isEmpty)
        throw new IllegalArgumentException("tagsToInclude was defined, but contained an empty set")
    case None =>
  }

  private def includedTestNames(testNamesAsList: List[String], tags: Map[String, Set[String]]): List[String] = 
    tagsToInclude match {
      case None => testNamesAsList
      case Some(tagsToInclude) =>
        for {
          testName <- testNamesAsList
          if tags contains testName
          intersection = tagsToInclude intersect tags(testName)
          if intersection.size > 0
        } yield testName
    }

  private def verifyPreconditionsForMethods(testNames: Set[String], tags: Map[String, Set[String]]) {
    val testWithEmptyTagSet = tags.find(tuple => tuple._2.isEmpty)
    testWithEmptyTagSet match {
      case Some((testName, _)) => throw new IllegalArgumentException(testName + " was associated with an empty set in the map passsed as tags")
      case None =>
    }
  }
  
  private def mergeTestTags(testTagsList: List[Map[String, Set[String]]]): Map[String, Set[String]] = {
    val mergedTags = scala.collection.mutable.Map[String, Set[String]]() ++ testTagsList.head
    for (testTags <- testTagsList.tail) {
      for ((testName, tagSet) <- testTags) {
        val existingTagSetOpt = mergedTags.get(testName)
        existingTagSetOpt match {
          case Some(existingTagSet) =>
            mergedTags(testName) = existingTagSet ++ tagSet
          case None => 
            mergedTags += ((testName, tagSet))
        }
      }
    }
    mergedTags.toMap
  }
  
  private[scalatest] def mergeTestDynamicTags(tags: Map[String, Set[String]], suiteId: String, testNames: Set[String]): Map[String, Set[String]] = {
    val dynaTestTags = 
      if (dynaTags.testTags.isDefinedAt(suiteId))
        dynaTags.testTags(suiteId)
      else
        Map.empty[String, Set[String]]
    
    val dynaSuiteTags = 
      if (dynaTags.suiteTags.isDefinedAt(suiteId)) {
        val suiteTags = dynaTags.suiteTags(suiteId)
        Map() ++ testNames.map(tn => (tn, suiteTags))
      }
      else
        Map.empty[String, Set[String]]
     
    mergeTestTags(List(tags, dynaTestTags, dynaSuiteTags))
  }

  /**
   * Filter test names based on their tags.
   *
   * <p>
   * Each tuple in the returned list contains a <code>String</code>
   * test name and a <code>Boolean</code> that indicates whether the test should be ignored. A test will be marked as ignored
   * if <code>org.scalatest.Ignore</code> is in its tags set, and either <code>tagsToInclude</code> is <code>None</code>, or
   * <code>tagsToInclude</code>'s value (a set) contains the test's name, unless another tag for that test besides <code>org.scalatest.Ignore</code>
   * is also included in <code>tagsToExclude</code>. For example, if a test is tagged with
   * both <code>org.scalatest.Ignore</code> and <code>SlowAsMolasses</code>, and <code>SlowAsMolasses</code>
   * appears in the <code>tagsToExclude</code> set, the <code>SlowAsMolasses</code> tag will
   * "overpower" the <code>org.scalatest.Ignore</code> tag, and this method will return
   * a list that does not include the test name.
   * </p>
   *
   * <pre class="stHighlight">
   * for ((testName, ignoreTest) <- filter(testNames, tags))
   *   if (ignoreTest)
   *     // ignore the test
   *   else
   *     // execute the test
   * </pre>
   *
   * @param testNames test names to be filtered
   * @param tags a map from test name to tags, containing only test names included in the <code>testNames</code> set, and
   *   only test names that have at least one tag
   *
   * @throws IllegalArgumentException if any set contained in the passed <code>tags</code> map is empty
   */
// I will make this private so I can keep using that darned deprecated implicit conversion.
// TODO: REMOVE THIS DEPRECATED ONCE TESTS PASS, AND AFTER DEPRECATION CYCLE OF THE Function2
// IMPLICIT, REMOVE THE WHOLE PRIVATE METHOD.
  @deprecated("Please use the apply method that takes a suiteId instead, the one with this signature: def apply(testNames: Set[String], testTags: Map[String, Set[String]], suiteId: String): List[(String, Boolean)]")
  private def apply(testNames: Set[String], tags: Map[String, Set[String]]): List[(String, Boolean)] = {

    verifyPreconditionsForMethods(testNames, tags)

    val testNamesAsList = testNames.toList // to preserve the order
    val filtered =
      for {
        testName <- includedTestNames(testNamesAsList, tags)
        if !tags.contains(testName) ||
                (tags(testName).contains(IgnoreTag) && (tags(testName) intersect (tagsToExclude + "org.scalatest.Ignore")).size == 1) ||
                (tags(testName) intersect tagsToExclude).size == 0
      } yield (testName, tags.contains(testName) && tags(testName).contains(IgnoreTag))

    filtered
  }
  
  def apply(testNames: Set[String], tags: Map[String, Set[String]], suiteId: String): List[(String, Boolean)] = {
    val testTags: Map[String, Set[String]] = mergeTestDynamicTags(tags, suiteId, testNames)
    verifyPreconditionsForMethods(testNames, testTags)

    val testNamesAsList = testNames.toList // to preserve the order
    val filtered =
      for {
        testName <- includedTestNames(testNamesAsList, testTags)
        if !testTags.contains(testName) ||
                (testTags(testName).contains(IgnoreTag) && (testTags(testName) intersect (tagsToExclude + "org.scalatest.Ignore")).size == 1) ||
                (testTags(testName) intersect tagsToExclude).size == 0
      } yield (testName, testTags.contains(testName) && testTags(testName).contains(IgnoreTag))

    filtered
  }

  /**
   * Filter one test name based on its tags.
   *
   * <p>
   * The returned tuple contains a <code>Boolean</code>
   * that indicates whether the test should be filtered, and if not, a <code>Boolean</code> that
   * indicates whether the test should be ignored. A test will be marked as ignored
   * if <code>org.scalatest.Ignore</code> is in its tags set, and either <code>tagsToInclude</code>
   * is <code>None</code>, or <code>tagsToInclude</code>'s value (a set) contains the passed
   * test name, unless another tag for that test besides <code>org.scalatest.Ignore</code>
   * is also included in <code>tagsToExclude</code>. For example, if a test is tagged with
   * both <code>org.scalatest.Ignore</code> and <code>SlowAsMolasses</code>, and <code>SlowAsMolasses</code>
   * appears in the <code>tagsToExclude</code> set, the <code>SlowAsMolasses</code> tag will
   * "overpower" the <code>org.scalatest.Ignore</code> tag, and this method will return
   * (true, false). 
   * </p>
   * 
   * <pre class="stHighlight">
   * val (filterTest, ignoreTest) = filter(testName, tags)
   * if (!filterTest)
   *   if (ignoreTest)
   *     // ignore the test
   *   else
   *     // execute the test
   * </pre>
   *
   * @param testName the test name to be filtered
   * @param tags a map from test name to tags, containing only test names that have at least one tag
   * @param suiteId the suite Id of the suite to filter
   *
   * @throws IllegalArgumentException if any set contained in the passed <code>tags</code> map is empty
   */
  def apply(testName: String, tags: Map[String, Set[String]], suiteId: String): (Boolean, Boolean) = {
    val testTags: Map[String, Set[String]] = mergeTestDynamicTags(tags, suiteId, Set(testName))
    val list = apply(Set(testName), testTags)
    if (list.isEmpty)
      (true, false)
    else
      (false, list.head._2)
  }

  /**
   * Returns the number of tests that should be run after the passed <code>testNames</code> and <code>tags</code> have been filtered
   * with the <code>tagsToInclude</code> and <code>tagsToExclude</code> class parameters.
   *
   * <p>
   * The result of this method may be smaller than the number of
   * elements in the list returned by <code>apply</code>, because the count returned by this method does not include ignored tests,
   * and the list returned by <code>apply</code> does include ignored tests.
   * </p>
   *
   * @param testNames test names to be filtered
   * @param tags a map from test name to tags, containing only test names included in the <code>testNames</code> set, and
   *   only test names that have at least one tag
   * @param suiteId the suite Id of the suite to filter
   *
   * @throws IllegalArgumentException if any set contained in the passed <code>tags</code> map is empty
   */
  def runnableTestCount(testNames: Set[String], testTags: Map[String, Set[String]], suiteId: String): Int = {
    val tags: Map[String, Set[String]] = mergeTestDynamicTags(testTags, suiteId, testNames)
    verifyPreconditionsForMethods(testNames, tags)

    val testNamesAsList = testNames.toList // to preserve the order
    val runnableTests = 
      for {
        testName <- includedTestNames(testNamesAsList, tags)
        if !tags.contains(testName) || (!tags(testName).contains(IgnoreTag) && (tags(testName) intersect tagsToExclude).size == 0)
      } yield testName

    runnableTests.size
  }
}

object Filter {
  private final val IgnoreTag = "org.scalatest.Ignore"

/**
 * Factory method for a <code>Filter</code> initialized with the passed <code>tagsToInclude</code>
 * and <code>tagsToExclude</code>.
 *
 * @param tagsToInclude an optional <code>Set</code> of <code>String</code> tag names to include (<em>i.e.</em>, not filter out) when filtering tests
 * @param tagsToExclude a <code>Set</code> of <code>String</code> tag names to exclude (<em>i.e.</em>, filter out) when filtering tests
 * @param excludeNestedSuites a <code>Boolean</code> to indicate whether to run nested suites
 * @param dynaTags dynamic tags for the filter
 *
 * @throws NullArgumentException if either <code>tagsToInclude</code> or <code>tagsToExclude</code> are null
 * @throws IllegalArgumentException if <code>tagsToInclude</code> is defined, but contains an empty set
 */
  def apply(tagsToInclude: Option[Set[String]] = None, tagsToExclude: Set[String] = Set(IgnoreTag), excludeNestedSuites: Boolean = false, dynaTags: DynaTags = DynaTags(Map.empty, Map.empty)) =
    new Filter(tagsToInclude, tagsToExclude, excludeNestedSuites, dynaTags)

  /**
   * Factory method for a default <code>Filter</code>, for which <code>tagsToInclude is <code>None</code>, 
   * <code>tagsToExclude</code> is <code>Set("org.scalatest.Ignore")</code>, and <code>excludeNestedSuites</code> is false.
   *
   * @return a default <code>Filter</code>
   */
  def default: Filter = apply()

  @deprecated("This implicit conversion was added in 2.3 because we dropped the inheritance relationship between Filter and Function2[Set[String], Map[String, Set[String]], List[(String, Boolean)]].")
  implicit def convertFilterToFunction2(filter: Filter): (Set[String], Map[String, Set[String]]) => List[(String, Boolean)] = (set, map) => filter.apply(set, map)
}
