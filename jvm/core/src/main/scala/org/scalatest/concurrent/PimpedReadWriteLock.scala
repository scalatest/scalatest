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
package org.scalatest.concurrent

import java.util.concurrent.locks.{Lock, ReadWriteLock}


/**
 * Provides implicit def for pimping a ReadWriteLock.
 * Provides a nice withLock method that has probably been written a million times.
 *
 * @author Josh Cough
 */
private[concurrent] object PimpedReadWriteLock {
  // SKIP-DOTTY-START
  import scala.language.implicitConversions

  /**
   * Implicit conversion that convert a <code>java.util.concurrent.locks.ReadWriteLock</code> to <code>PimpedReadWriteLock</code>.
   *
   * @param lock the <code>ReadWriteLock</code>
   * @return an instance of <code>PimpedReadWriteLock</code>
   */
  implicit def pimpMyReadWriteLock(lock: ReadWriteLock): PimpedReadWriteLock = new PimpedReadWriteLock(lock)
  // SKIP-DOTTY-END

  //DOTTY-ONLY /**
  //DOTTY-ONLY  * Convert a <code>java.util.concurrent.locks.ReadWriteLock</code> to <code>PimpedReadWriteLock</code>.
  //DOTTY-ONLY  *
  //DOTTY-ONLY  * @param lock the <code>ReadWriteLock</code>
  //DOTTY-ONLY  * @return an instance of <code>PimpedReadWriteLock</code>
  //DOTTY-ONLY  */
  //DOTTY-ONLY def pimpMyReadWriteLock(lock: ReadWriteLock): PimpedReadWriteLock = new PimpedReadWriteLock(lock)

  //DOTTY-ONLY /**
  //DOTTY-ONLY   * Extension methods for <code>java.util.concurrent.locks.ReadWriteLock</code> to support methods of <code>PimpedReadWriteLock</code>.
  //DOTTY-ONLY   */
  //DOTTY-ONLY extension (lock: ReadWriteLock) {
  //DOTTY-ONLY   /**
  //DOTTY-ONLY    * Locks the read lock
  //DOTTY-ONLY    * Executes the given function, holding the result
  //DOTTY-ONLY    * Unlocks the read lock
  //DOTTY-ONLY    *
  //DOTTY-ONLY    * @param f the function to be executed while the read lock is locked
  //DOTTY-ONLY    */
  //DOTTY-ONLY   def withReadLock[T](f: => T): T = pimpMyReadWriteLock(lock).withReadLock(f)
  //DOTTY-ONLY   /*
  //DOTTY-ONLY    * Alternate name for withReadLock
  //DOTTY-ONLY    */
  //DOTTY-ONLY   def read[T](f: => T): T = pimpMyReadWriteLock(lock).read(f)
  //DOTTY-ONLY   /**
  //DOTTY-ONLY    * Locks the write lock
  //DOTTY-ONLY    * Executes the given function, holding the result
  //DOTTY-ONLY    * Unlocks the write lock
  //DOTTY-ONLY    * Returns the result
  //DOTTY-ONLY    *
  //DOTTY-ONLY    * @param f the function to be executed while the read lock is locked
  //DOTTY-ONLY    */
  //DOTTY-ONLY   def withWriteLock[T](f: => T): T = pimpMyReadWriteLock(lock).withWriteLock(f)
  //DOTTY-ONLY   /*
  //DOTTY-ONLY    * Alternate name for withWriteLock
  //DOTTY-ONLY    * Returns the result
  //DOTTY-ONLY    */
  //DOTTY-ONLY   def write[T](f: => T): T = pimpMyReadWriteLock(lock).write(f)
  //DOTTY-ONLY }

  /**
   * Locks the given lock
   * Executes the given function, holding the result
   * Unlocks the lock
   * Returns the result
   *
   * @param lock the lock to be locked while executing the given function
   * @param f the function to be executed while the lock is locked
   */
  def withLock[T](lock: Lock)(f: => T): T = {
    lock.lock
    val t =
      try {
        f
      }
      finally {
        lock.unlock
      }
    t
  }
}

/**
 * Adds withReadLock and withWriteLock functions to ReadWriteLock.
 */
private[concurrent] class PimpedReadWriteLock(lock: ReadWriteLock) {

  /**
   * Locks the read lock
   * Executes the given function, holding the result
   * Unlocks the read lock
   *
   * @param f the function to be executed while the read lock is locked
   */
  def withReadLock[T](f: => T): T = PimpedReadWriteLock.withLock(lock.readLock) {f}

  /*
   * Alternate name for withReadLock
   */
  def read[T](f: => T): T = PimpedReadWriteLock.withLock(lock.readLock) {f}

  /**
   * Locks the write lock
   * Executes the given function, holding the result
   * Unlocks the write lock
   * Returns the result
   *
   * @param f the function to be executed while the read lock is locked
   */
  def withWriteLock[T](f: => T): T = PimpedReadWriteLock.withLock(lock.writeLock) {f}

  /*
   * Alternate name for withWriteLock
   * Returns the result
   */
  def write[T](f: => T): T = PimpedReadWriteLock.withLock(lock.writeLock) {f}
}
 
