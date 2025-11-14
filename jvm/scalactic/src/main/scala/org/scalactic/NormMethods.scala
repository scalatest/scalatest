/*
 * Copyright 2001-2025 Artima, Inc.
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
package org.scalactic

/**
 * Provides an implicit conversion that allows <code>norm</code> to be invoked on any value of type
 * <code>T</code> for which an implicit <code>Normalization[T]</code> exists.
 *
 * <p>
 * Here's an example:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; import org.scalactic._
 * import org.scalactic._
 *
 * scala&gt; import StringNormalizations._
 * import StringNormalizations._
 *
 * scala&gt; implicit val stringNormalization = lowerCased and trimmed
 * stringNormalization: org.scalactic.Uniformity[String] = org.scalactic.Uniformity$$anon$1@19ba67ec
 *
 * scala&gt; import NormMethods._
 * import NormMethods._
 *
 * scala&gt; val s = " There "
 * s: String = " There "
 *
 * scala&gt; "Hey " + s + "!"
 * res5: String = Hey  There !
 *
 * scala&gt; "Hey " + s.norm + "!"
 * res6: String = Hey there!
 * </pre>
 */
trait NormMethods {

  import NormMethods.Normalizer

  // SKIP-DOTTY-START
  import scala.language.implicitConversions
  // SKIP-DOTTY-END

  /**
  // SKIP-DOTTY-START
   * Implicit conversion that adds a <code>norm</code> method to a value of any type <code>T</code> for which
   * an implicit <code>Normalization[T]</code> exists.
  // SKIP-DOTTY-END 
  //DOTTY-ONLY * Conversion that adds a <code>norm</code> method to a value of any type <code>T</code>  
  //DOTTY-ONLY * using an existing <code>Normalization[T]</code>.
   *
   * @param o the object to convert
   * @return a <a href="Normalizer.html"><code>Normalizer</code></a> that enables a <code>norm</code> method to be invoked on the passed object
   */
  // SKIP-DOTTY-START 
  implicit def convertToNormalizer[T](o: T)(implicit normalization: Normalization[T]): Normalizer[T] = new Normalizer[T](o)
  // SKIP-DOTTY-END
  //DOTTY-ONLY def convertToNormalizer[T](o: T)(using normalization: Normalization[T]): Normalizer[T] = new Normalizer[T](o)
  //DOTTY-ONLY extension [T](o: T)(using normalization: Normalization[T]) {
  //DOTTY-ONLY   def norm: T = convertToNormalizer(o).norm
  //DOTTY-ONLY }
}

/**
 * Companion object for <code>NormMethods</code> enabling its members to be imported as an alternative to mixing them in.
 */
object NormMethods extends NormMethods {
  /**
   * Class containing a <code>norm</code> method that normalizes the given object <code>o</code> of type <code>T</code>
   * via the implicitly passed <code>Normalization[T]</code>.
   */
  // SKIP-DOTTY-START 
  final class Normalizer[T](o: T)(implicit normalization: Normalization[T]) {
  // SKIP-DOTTY-END
  //DOTTY-ONLY final class Normalizer[T](o: T)(using normalization: Normalization[T]) {
    /**
     * Normalizes the object <code>o</code> of type <code>T</code> via the implicitly passed <code>Normalization[T]</code> passed
     * to the constructor of this <code>Normalizer</code>.
     *
     * @return a normalized form of <code>o</code>
     */
    def norm: T = normalization.normalized(o)
  }
}

