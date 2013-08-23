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

/**
 * <b>Trait <code>FailureOf</code> has been deprecated and will be removed in a future version of ScalaTest.
 * Instead of the <code>failureOf</code> method provided by this trait, please use the <code>outcomeOf</code> method
 * provided by trait <code>OutcomeOf</code> instead.</b> 
 *
 * <p>
 * The <code>outcomeOf</code> method is not just the <code>failureOf</code> method with the name changed, so you will likely
 * need to do a bit of rewriting of your usage of <code>failureOf</code>. The difference is that <code>failureOf</code> 
 * returned an optional <code>Throwable</code>, whereas <code>outcomeOf</code> returns an <a href="Outcome.html"><code>Outcome</code></a>.
 * </p>
 *
 * @author Bill Venners
 */
@deprecated("The FailureOf trait has been deprecated. Please use trait OutcomeOf instead.")
trait FailureOf {

  @deprecated("The failureOf method of trait FailureOf has been deprecated. Please use the outcomeOf method of trait OutcomeOf instead.")
  def failureOf(f: => Unit): Option[Throwable] = {
    
    try {                                         
      f                                           
      None                                        
    }                                             
    catch {                                       
      case e: Throwable =>
        if (!Suite.anExceptionThatShouldCauseAnAbort(e))
          Some(e)                           
        else
          throw e
    }
  }
}

@deprecated("The FailureOf object has been deprecated. Please use object OutcomeOf instead.")
object FailureOf extends FailureOf
