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
package org.scalatest.tags;

import java.lang.annotation.*;
import org.scalatest.TagAnnotation;

/**
 * Annotation used to tag a test, or suite of tests, as being CPU-intensive (<em>i.e.</em>, consuming a lot of CPU time when it runs).
 *
 * <p>
 * <em>Note: This is actually an annotation defined in Java, not a Scala trait. It must be defined in Java instead of Scala so it will be accessible
 * at runtime. It has been inserted into Scaladoc by pretending it is a trait.</em>
 * </p>
 *
 * <p>
 * If you wish to mark an entire suite of tests as being CPU-intensive, you can annotate the test class with <code>@CPU</code>, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.flatspec.cpuall
 * 
 * import org.scalatest._
 * import tags.CPU
 * 
 * @CPU
 * class SetSpec extends FlatSpec {
 * 
 *   "An empty Set" should "have size 0" in {
 *     assert(Set.empty.size === 0)
 *   }
 * 
 *   it should "produce NoSuchElementException when head is invoked" in {
 *     intercept[NoSuchElementException] {
 *       Set.empty.head
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * When you mark a test class with a tag annotation, ScalaTest will mark each test defined in that class with that tag.
 * Thus, marking the <code>SetSpec</code> in the above example with the <code>@CPU</code> tag annotation means that both tests
 * in the class are CPU-intensive.
 * </p>
 *
 * <p>
 * Another use case for <code>@CPU</code> is to mark test <em>methods</em> as CPU-intensive in traits <a href="Spec.html"><code>Spec</code></a>
 * and <a href="fixture/Spec.html"><code>fixture.Spec</code></a>. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.spec.cpu
 *
 * import org.scalatest._
 * import tags.CPU
 *
 * class SetSpec extends RefSpec {
 *
 *   @CPU def &#96;an empty Set should have size 0&#96; {
 *     assert(Set.empty.size === 0)
 *   }
 *
 *   def &#96;invoking head on an empty Set should produce NoSuchElementException&#96; {
 *     intercept[NoSuchElementException] {
 *       Set.empty.head
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * The main use case of annotating a test or suite of tests is to select or deselect them during runs by supplying tags to include and/or exclude. For more information,
 * see the relevant section in the documentation of object <a href="../tools/Runner$.html#specifyingTagsToIncludeAndExclude"><code>Runner</code></a>.
 * </p>
 * 
 * <p>
 * Note that because reflection is not supported on Scala.js, this annotation will only work on the JVM, not on Scala.js.
 * </p>
 */
@TagAnnotation("cpu")
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE})
@Inherited
public @interface CPU {}
