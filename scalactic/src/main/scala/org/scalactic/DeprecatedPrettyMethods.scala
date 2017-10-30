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
 *
 * <strong>This trait is the 2.2.6 PrettyMethods trait renamed to DeprecatedPrettyMethods. It is
 * a quick way to get old code working again that defined an implicit <code>PrettifierConfig</code>, but
 * before the deprecation cycle is over please change the implicit <code>PrettifierConfig</code> to
 * an implicit <code>Prettifier</code>.</strong>
 * Provides an implicit conversion that enables <code>pretty</code> to be invoked on any
 * object, to transform that object into a <code>String</code> representation.
 */
@deprecated("Please use PrettyMethods instead.")
trait DeprecatedPrettyMethods {

  /**
   * <strong>This class is deprecated and will be removed in a future version of ScalaTest. Please use an
   * implicit Prettifier and PrettyMethods instead.</strong>
   *
   * Wraps a <code>Prettifier</code>.
   *
   * <p>
   * This class exists so that instances of <code>PrettifierConfig</code> can be made implicit instead
   * of <code>Prettifer</code>. Because <code>Prettifier</code> is a <code>Any =&gt; String</code>, 
   * making it implicit could result in unintentional applications.
   * </p>
   *
   * @param prettifier the configured <code>Prettifier</code>
   */
  @deprecated("Please use Prettifer and PrettyMethods instead.")
  case class PrettifierConfig(prettifier: Prettifier)

  /**
   * An implicit <code>PrettifierConfig</code> that contains a <code>Prettifier.default</code>.
   *
   * <p>
   * Subclasses can override this method with a different implicit method to have <code>pretty</code>
   * use a different <code>Prettifier</code>.
   * </p>
   */
  implicit val prettifierConfig: PrettifierConfig = PrettifierConfig(Prettifier.default)

  /**
   * Implicit class that adds a <code>pretty</code> method to any object.
   *
   * <p>
   * The constructor of this class, besides taking an object <code>o</code> to prettify,
   * also takes an implicit <code>PrettifierConfig</code> that the <code>pretty</code> method will use to prettify the
   * object.
   * </p>
   *
   * @param o the object to prettify
   * @param prettifierConfig an implicit <code>PrettifierConfig</code> whose <code>Prettifier</code> will be used
   *     to prettify the passed object <code>o</code>
   */
  implicit class Prettyizer(o: Any)(implicit prettifierConfig: PrettifierConfig) {

    /**
     * Returns a pretty <code>String</code> representation of the object <code>o</code>
     */
    def pretty: String = prettifierConfig.prettifier(o)
  }
}

/**
 * <strong>This object is the 2.2.6 PrettyMethods object renamed to DeprecatedPrettyMethods. Please change
 * DeprecatedPrettyMethods to PrettyMethods, as DeprecatedPrettyMethods is deprecated and will be removed
 * in a future version of ScalaTest.</strong>
 */
@deprecated("Please use PrettyMethods instead.")
object DeprecatedPrettyMethods extends DeprecatedPrettyMethods

