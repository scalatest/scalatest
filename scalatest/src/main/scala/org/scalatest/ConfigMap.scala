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

import exceptions.TestCanceledException
import reflect.ClassManifest
import collection.immutable.MapLike
import org.scalactic.Equality
import enablers.Containing
import enablers.Aggregating
import enablers.KeyMapping
import enablers.ValueMapping
import scala.collection.GenTraversable

// TODO: Oops. Need to pass ConfigMap not Map[String, Any] in TestStarting.
/**
 * A map of configuration data.
 *
 * <p>
 * A <code>ConfigMap</code> can be populated from the <a href="tools/Runner$.html"><code>Runner</code></a> command line via <code>-D</code> 
 * arguments. <code>Runner</code> passes it to many methods where you can use it to configure your
 * test runs. For example, <code>Runner</code> passed the <code>ConfigMap</code> to:
 * </p>
 * 
 * <ul>
 * <li>the <code>apply</code> method of <a href="Reporter.html"><code>Reporter</code></a>s via <code>RunStarting</code> events</li>
 * <li>the <code>run</code> method of <a href="Suite.html"><code>Suite</code></a>
 * <li>the <code>runNestedSuites</code> method of <code>Suite</code>
 * <li>the <code>runTests</code> method of <code>Suite</code>
 * <li>the <code>runTest</code> method of <code>Suite</code>
 * <li>the <code>withFixture(NoArgTest)</code> method of <code>Suite</code>
 * <li>the <code>withFixture(OneArgTest)</code> method of <a href="fixture/Suite.html"><code>fixture.Suite</code></a>
 * <li>the <code>beforeEach(TestData)</code> method of <a href="BeforeAndAfterEachTestData.html"><code>BeforeAndAfterEachTestData</code></a>
 * <li>the <code>afterEach(TestData)</code> method of <code>BeforeAndAfterEachTestData</code>
 * </ul>
 *
 * <p>
 * In addition to accessing the <code>ConfigMap</code> in overriden implementations of the above methods, you can also transform
 * and pass along a modified <code>ConfigMap</code>.
 * </p>
 *
 * <p>
 * A <code>ConfigMap</code> maps string keys to values of any type, <em>i.e.</em>, it is a <code>Map[String, Any]</code>.
 * To get a configuration value in a variable of the actual type of that value, therefore, you'll need to perform an unsafe cast. If
 * this cast fails, you'll get an exception, which so long as the <code>ConfigMap</code> is used only in tests, will
 * result in either a failed or canceled test or aborted suite. To give such exceptions nice stack depths and error messages, and to
 * eliminate the need for using <code>asInstanceOf</code> in your test code, <code>ConfigMap</code> provides three
 * methods for accessing values at expected types.
 * </p>
 *
 * <p>
 * The <code>getRequired</code> method returns the value bound to a key cast to a specified type, or throws <a href="exceptions/TestCanceledException.html"><code>TestCanceledException</code></a>
 * if either the key is not bound or is bound to an incompatible type. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * val tempFileName: String = configMap.getRequired[String]("tempFileName")
 * </pre>
 *
 * <p>
 * The <code>getOptional</code> method returns the value bound to a key cast to a specified type, wrapped in a <code>Some</code>,
 * returns <code>None</code> if the key is not bound, or throws </code>TestCanceledException</code> if the key exists but is
 * bound to an incompatible type. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * val tempFileName: Option[String] = configMap.getOptional[String]("tempFileName")
 * </pre>
 *
 * <p>
 * The <code>getWithDefault</code> method returns the value bound to a key cast to a specified type,
 * returns a specified default value if the key is not bound, or throws </code>TestCanceledException</code> if the key exists but is
 * either not bound or is bound to an incompatible type. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * val tempFileName: String = configMap.getWithDefault[String]("tempFileName", "tmp.txt")
 * </pre>
 *
 * @param underlying an immutable <code>Map</code> that holds the key/value pairs contained in this <code>ConfigMap</code>
 * 
 * @author Bill Venners
 */
class ConfigMap(underlying: Map[String, Any]) extends Map[String, Any] with MapLike[String, Any, ConfigMap] with java.io.Serializable {

  def get(key: String): Option[Any] = underlying.get(key)

  def iterator: Iterator[(String, Any)] = underlying.iterator

  def +[A >: Any](kv: (String, A)): ConfigMap = new ConfigMap(underlying + kv)

  def -(key: String): ConfigMap = new ConfigMap(underlying - key)

  override def empty: ConfigMap = new ConfigMap(Map.empty[String, Any])

  /**
   * <p>
   * Returns the value bound to a key cast to a specified type, wrapped in a <code>Some</code>,
   * returns <code>None</code> if the key is not bound, or throws </code>TestCanceledException</code> if the key exists but is
   * bound to an incompatible type. Here's an example:
   * </p>
   *
   * <pre class="stHighlight">
   * val tempFileName: Option[String] = configMap.getOptional[String]("tempFileName")
   * </pre>
   *
   * @param key the key with which the desired value should be associated
   * @param manifest an implicit <code>ClassManifest</code> specifying the expected type for the desired value
   */
  def getOptional[V](key: String)(implicit manifest: ClassManifest[V]): Option[V] = {
    if (underlying.contains(key)) Some(getRequired[V](key))
    else None
  }

  /**
   * <p>
   * Returns the value bound to a key cast to the specified type <code>V</code>,
   * returns a specified default value if the key is not bound, or throws </code>TestCanceledException</code> if the key exists but is
   * if either the key is not bound or is bound to an incompatible type. Here's an example:
   * </p>
   *
   * <pre class="stHighlight">
   * val tempFileName: String = configMap.getWithDefault[String]("tempFileName", "tmp.txt")
   * </pre>
   *
   * @param key the key with which the desired value should be associated
   * @param default a default value to return if the key is not found
   * @param manifest an implicit <code>ClassManifest</code> specifying the expected type for the desired value
   */
  def getWithDefault[V](key: String, default: => V)(implicit manifest: ClassManifest[V]): V = {
    if (underlying.contains(key)) getRequired[V](key)
    else default
  }

  /**
   * <p>
   * Returns the value bound to a key cast to the specified type <code>V</code>, or throws <code>TestCanceledException</code>
   * if either the key is not bound or is bound to an incompatible type. Here's an example:
   * </p>
   *
   * <pre class="stHighlight">
   * val tempFileName: String = configMap.getRequired[String]("tempFileName")
   * </pre>
   *
   * @param key the key with which the desired value should be associated
   * @param manifest an implicit <code>ClassManifest</code> specifying the expected type for the desired value
   */
  def getRequired[V](key: String)(implicit manifest: ClassManifest[V]): V = {
    underlying.get(key) match {
      case Some(value) =>
        val expectedClass = manifest.erasure
        val boxedExpectedClass =
          expectedClass match {
            case java.lang.Boolean.TYPE => classOf[java.lang.Boolean]
            case java.lang.Byte.TYPE => classOf[java.lang.Byte]
            case java.lang.Short.TYPE => classOf[java.lang.Short]
            case java.lang.Integer.TYPE => classOf[java.lang.Integer]
            case java.lang.Long.TYPE => classOf[java.lang.Long]
            case java.lang.Character.TYPE => classOf[java.lang.Character]
            case java.lang.Float.TYPE => classOf[java.lang.Float]
            case java.lang.Double.TYPE => classOf[java.lang.Double]
            case _ => expectedClass
          }
        val actualClass = value.asInstanceOf[AnyRef].getClass
        // if (actualClass.isAssignableFrom(boxedExpectedClass))
        if (boxedExpectedClass.isAssignableFrom(actualClass))
          value.asInstanceOf[V]
        else
            throw new TestCanceledException(Resources("configMapEntryHadUnexpectedType", key, actualClass, expectedClass, value.asInstanceOf[AnyRef]), 1) // TODO: Fix stack depth
      case None => throw new TestCanceledException(Resources("configMapEntryNotFound", key), 1) // TODO: Fix stack depth
    }
  }
}

/**
 * Companion object to class <code>ConfigMap</code> containing factory methods.
 *
 * @author Bill Venners
 */
object ConfigMap {

  /**
   * Constructs a <code>ConfigMap</code> containing the passed key/value pairs.
   *
   * @param pairs zero to many key/value pairs with which to initialize a new <code>ConfigMap</code>.
   */
  def apply(pairs: (String, Any)*): ConfigMap = new ConfigMap(Map(pairs: _*))

  /**
   * Constructs an empty <code>ConfigMap</code>.
   */
  def empty: ConfigMap = new ConfigMap(Map.empty)
}

