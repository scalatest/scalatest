/*
 * Copyright 2001-2012 Artima, Inc.
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

/**
 * Dynamic tags for a run.
 *
 * <p>
 * Instances of this class are passed to the <a href="Filter.html"><code>Filter</code></a> constructor to
 * support running selected suites and tests via dynamic tagging. For example, dynamic tags can be used
 * to rerun tests that failed previously, or tests selected via a wildcard from <a href="tools/Runner$.html"><code>Runner</code></a> or
 * the Scala interpreter.
 * </p>
 *
 * @param suiteTags a map from String suite ID to a set of tags for that suite.
 * @param testTags a map from String suite ID to a map, whose keys are test names and values the tags for that test.
 * @throws NullPointerException if either <code>suiteTags</code> or <code>testTags</code> is <code>null</code>
 *
 */
final case class DynaTags(suiteTags: Map[String, Set[String]], testTags: Map[String, Map[String, Set[String]]]) {
  if (suiteTags == null)
    throw new NullPointerException("suiteTags was null")
  if (testTags == null)
    throw new NullPointerException("testTags was null")
}
