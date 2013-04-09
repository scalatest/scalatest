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
package org.scalautils

import org.scalatest._
import scala.collection.GenSeq
import scala.collection.GenMap
import scala.collection.GenSet
import scala.collection.GenIterable
import scala.collection.GenTraversable
import scala.collection.GenTraversableOnce

class XmlEqualitySpec extends Spec with TripleEquals {

  object `the XmlEquality type class` {

    def `should call .equals on the left hand object (and not on the right hand object)` {

      assert(<a href="hi"></a> == <a href="hi"></a>)
      assert(<a href="hi"></a> != <a href="hi"> </a>)

      assert(<a href="hi"></a> === <a href="hi"></a>)
      assert(<a href="hi"></a> !== <a href="hi"> </a>)

      new XmlEquality {
        assert(<a href="hi"></a> === <a href="hi"></a>)
        // assert(<a href="hi"></a>.asInstanceOf[xml.Node] === <a href="hi"> </a>)
        assert(<a href="hi"></a> === <a href="hi"> </a>)
      }
    }
  }
}

