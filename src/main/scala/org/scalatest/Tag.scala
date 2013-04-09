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
package org.scalatest

/**
 * Class whose subclasses can be used to tag tests in style traits in which tests are defined as functions.
 *
 * <p>
 * ScalaTest has two ways to tag tests: annotations and instances of this <code>Tag</code> class.
 * To tag a test method or an entire test class, you use a <em>tag annotation</em>, whereas to tag a test function,
 * you use a <code>Tag</code> object. Though not required, it is usually a good idea to define both an annotation
 * and a corresponding <code>Tag</code> object for each conceptual tag you want, so you can tag anything: test functions, test classes,
 * and test methods. The name of the conceptual tag is the fully qualified name of the annotation interface, so you must
 * pass this name to the <code>Tag</code> constructor.
 * </p>
 *
 * <p>
 * For example, imagine you want to tag integration tests that use the actual database, and are, therefore, generally slower. You could
 * create a tag annotation and object called <code>DbTest</code>. To give them both the same simple name, you can declare them in different packages.
 * The tag annotation must be written in Java, not Scala, because annotations written
 * in Scala are not accessible at runtime. Here's an example:
 * </p>
 *
 * <pre>
 * package com.mycompany.myproject.testing.tags;
 *
 * import java.lang.annotation.*; 
 * import org.scalatest.TagAnnotation
 *
 * @TagAnnotation
 * @Retention(RetentionPolicy.RUNTIME)
 * @Target({ElementType.METHOD, ElementType.TYPE})
 * public @interface DbTest {}
 * </pre>
 *
 * <p>
 * Given this annotation's fully qualified name is <code>com.mycompany.myproject.testing.tags.DbTest</code> the corresponding <code>Tag</code>
 * object decaration must have that name passed to its constructor, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * package com.mycompany.myproject.testing.tagobjects
 *
 * object DbTest extends Tag("com.mycompany.myproject.testing.tags.DbTest")
 * </pre>
 *
 * <p>
 * Given these definitions, you could tag a test function as a <code>DbTest</code> in, for
 * example, a <a href="FlatSpec.html"><code>FlatSpec</code></a> like this:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.FlatSpec
 * import com.mycompany.myproject.testing.tagobjects.DbTest
 *
 * class ExampleSpec extends FlatSpec {
 *
 *   "Integration tests" can "sometimes be slow" taggedAs(DbTest) in {
 *     Thread.sleep(1000)
 *   }
 * }
 * </pre>
 *
 * <p>
 * You could tag a test method as a <code>DbTest</code> in, for
 * example, a <a href="Suite.html"><code>Suite</code></a> like this:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.Suite
 * import com.mycompany.myproject.testing.tags.DbTest
 *
 * class ExampleSuite extends Suite {
 *
 *   @DbTest
 *   def &#96;integration tests can sometimes be slow&#96; {
 *     Thread.sleep(1000)
 *   }
 * }
 * </pre>
 *
 * <p>
 * And you could tag all the tests in an entire test class by annotating the class, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.FlatSpec
 * import com.mycompany.myproject.testing.tags.DbTest
 *
 * @DBTest
 * class ExampleSpec extends FlatSpec {
 *
 *   "Integration tests" can "sometimes be slow" in {
 *     Thread.sleep(1000)
 *   }
 *
 *   they should "likely sometimes be excluded " in {
 *     Thread.sleep(1000)
 *   }
 * }
 * </pre>
 *
 * <p>
 * In the previous example, both tests will be tagged as <code>DBTest</code>s even though the
 * tests are not tagged as such individually. 
 * </p>
 *
 * <p>
 * When you run ScalaTest and want to either include or exclude <code>DbTest</code>s, you'd give the fully qualified
 * name of the tag annotation (which is also the name passed to the corresponding <code>Tag</code> constructor) to <a href="tools/Runner$.html"><code>Runner</code></a>. For
 * example, here's how you'd exclude <code>DbTest</code>s on the <code>Runner</code> command line:
 * </p>
 *
 * <pre>
 * -l com.mycompany.myproject.testing.tags.DbTest
 * </pre>
 *
 * <p>
 * For examples of tagging in other style traits, see the "Tagging tests" section in the documentation for the trait:
 * </p>
 *
 * <ul>
 * <li><a href="FeatureSpec.html#taggingTests">Tagging <code>FeatureSpec</code> tests</a></li>
 * <li><a href="FlatSpec.html#taggingTests">Tagging <code>FlatSpec</code> tests</a></li>
 * <li><a href="FreeSpec.html#taggingTests">Tagging <code>FreeSpec</code> tests</a></li>
 * <li><a href="FunSpec.html#taggingTests">Tagging <code>FunSpec</code> tests</a></li>
 * <li><a href="FunSuite.html#taggingTests">Tagging <code>FunSuite</code> tests</a></li>
 * <li><a href="PropSpec.html#taggingTests">Tagging <code>PropSpec</code> tests</a></li>
 * <li><a href="Suite.html#taggingTests">Tagging <code>Suite</code> tests</a></li>
 * <li><a href="WordSpec.html#taggingTests">Tagging <code>WordSpec</code> tests</a></li>
 * </ul>
 *
 * @author Bill Venners
 * @author George Berger
 */
class Tag(val name: String)

/**
 * Companion object for <code>Tag</code>, which offers a factory method.
 *
 * @author George Berger
 * @author Bill Venners
 */
object Tag {

  /**
   * Factory method for creating new <code>Tag</code> objects.
   */
  def apply(name: String): Tag = {
    new Tag(name)
  }
}

