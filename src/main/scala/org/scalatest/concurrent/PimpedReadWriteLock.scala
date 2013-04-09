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
package org.scalatest.concurrent

import org.scalatest._
import java.util.concurrent.locks.{Lock, ReadWriteLock}

/**
 * Provides implicit def for pimping a ReadWriteLock.
 * Provides a nice withLock method that has probably been written a million times.
 *
 * @author Josh Cough
 */
private[concurrent] object PimpedReadWriteLock {
  implicit def pimpMyReadWriteLock(lock: ReadWriteLock) = new PimpedReadWriteLock(lock)

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
 
