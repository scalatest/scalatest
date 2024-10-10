/*
 * Copyright 2001-2017 Artima, Inc.
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
package org.scalatest.prop

/**
  * Describes the outcome of a property check operation such as 'forAll()'.
  *
  * This will always be one of three subtypes: [[org.scalatest.prop.PropertyCheckResult.Success]],
  * [[org.scalatest.prop.PropertyCheckResult.Exhausted]], or [[org.scalatest.prop.PropertyCheckResult.Failure]].
  * See those subclasses for details on what each one means.
  */
sealed trait PropertyCheckResult

object PropertyCheckResult {

  /**
    * This property check was successful -- after running for the desired number of times, it was not falsified.
    *
    * Note that "not falsified" does not mean the same thing as "is definitely correct". The check code strives 
    * to test edge cases that are likely to detect errors, but there are exceptions, and the check is only as
    * good as the property you have defined. But if the property accurately describes the problem, this usually
    * signifies that things look good.
    *
    * @param args The arguments passed into this check.
    * @param initSeed The random seed used for this check. This seed is used by the checking code, to pass in to
    *                 calls to [[Randomizer]]. Reusing the same seed for subsequent runs should produce the same
    *                 results, so this can be valuable debugging information if you are seeing inconsistent results --
    *                 this provides a seed that passed the check.
    */
  case class Success(args: List[PropertyArgument], initSeed: Long) extends PropertyCheckResult

  /**
    * This Property Check was inconclusive -- after running for the desired number of times, so many attempts were
    * discarded that the property as defined should be considered suspect.
    *
    * Note that this does *not* mean that the check failed. No falsifying examples were found. The problem is that
    * the property rejected so many of the generated parameters that it looks a bit iffy.
    *
    * TODO: describe how constraints work in this check environment, and precisely how discarding works.
    *
    * @param succeeded How many generated values succeeded.
    * @param discarded How many generated values were rejected as not satisfying the property's preconditions.
    * @param names TODO: how does this parameter different from argsPassed?
    * @param argsPassed The arguments passed into this check.
    * @param initSeed The random seed used for this check. This seed is used by the checking code, to pass in to
    *                 calls to [[Randomizer]]. Reusing the same seed for subsequent runs should produce the same
    *                 results, so this can be valuable debugging information if you are seeing inconsistent results --
    *                 this provides a seed that produced indeterminate results.
    */
  case class Exhausted(succeeded: Long, discarded: Long, names: List[String], argsPassed: List[PropertyArgument], initSeed: Long) extends PropertyCheckResult

  /**
    * This Property Check did not succeed -- the property was falsified.
    *
    * This generally indicates a bug in your code. Generated values that produced an error.
    * The next step is usually to examine these values, and understand why they caused an error.
    *
    * Note that this does not ``necessarily`` mean that the code under test is in error: the property 
    * itself might be incorrect. It is good practice to check both, and figure out which one is wrong.
    *
    * @param succeeded The number of generated values passed the property before the failure.
    * @param ex The exception that was thrown by the property, if any.
    * @param names TODO: how does this parameter differ from argsPassed?
    * @param argsPassed The arguments passed into this check.
    * @param initSeed The random seed used for this check. This seed is used by the checking code, to pass into
    *                 calls to [[Randomizer]]. Reusing the same seed for subsequent runs should produce the same
    *                 results, so this can be extremely valuable while debugging the error: re-running the property
    *                 with the same seed should retry the test with exactly the same text data, so you can easily
    *                 test your fixes.
    */
  case class Failure(succeeded: Long, ex: Option[Throwable], names: List[String], argsPassed: List[PropertyArgument], initSeed: Long) extends PropertyCheckResult

}
