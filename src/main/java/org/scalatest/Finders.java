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

package org.scalatest;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotation used to mark a trait or class as defining a testing style that has a <code>org.scalatest.finders.Finder</code> implementation,
 * which IDEs and other tools can use to discover tests and scopes.
 *
 * <p>
 * <em>Note: This is actually an annotation defined in Java, not a Scala trait. It must be defined in Java instead of Scala so it will be accessible
 * at runtime. It has been inserted into Scaladoc by pretending it is a trait.</em>
 * </p>
 *
 * <p>
 * This annotation is used to enable different styles of testing, including both native ScalaTest styles and custom user-created styles, to
 * have rich IDE support. The "Finder API" is released separately from ScalaTest proper, because it is only used by tools such as IDEs.
 * Documentation on the finder API itself will appear later, as we get closer to the 2.0 final release.
 * </p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface Finders {
    String[] value();
}

