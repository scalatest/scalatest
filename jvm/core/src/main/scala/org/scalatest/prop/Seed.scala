package org.scalatest.prop

/*
 * Copyright 2001-2024 Artima, Inc.
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

/**
 * Seed data class holding seed value used for generating random value.
 */
final case class Seed(value: Long)

/**
 * Companion object for Seed class.
 */
object Seed {

  import java.util.concurrent.atomic.AtomicReference

  /**
    * This seed is empty under ordinary circumstances. It is here so that the test
    * Runner can poke in a seed value to be used during a test run. If set, it will be used
    * as the seed for all calls to [[Seed.default]].
    */
  private[scalatest] val configuredRef: AtomicReference[Option[Long]] = new AtomicReference(None)

  /**
   * Creates a new Seed using default approach, which is initialized based on the current time.
   *
   * This should not be considered a strong source of seed for randomness -- in cases where high entropy really
   * matters, it's a bit mediocre -- but for general purposes it's typically good enough.
   */
  def default: Seed = 
    Seed(
      configuredRef.get() match {
        case Some(value) => value
        case None => System.currentTimeMillis()
      }
    )

  def configured: Option[Seed] = configuredRef.get().map(Seed(_))  
}