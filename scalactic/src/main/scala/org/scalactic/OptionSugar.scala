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
trait OptionSugar {

  implicit class Optionizer[G](option: Option[G]) {
    def toOr[B](orElse: B): G Or B = Or.from(option, orElse)
  }
} 

/**
 * Companion object for <code>NormMethods</code> enabling its members to be imported as an alternative to mixing them in.
 */
object OptionSugar extends OptionSugar

