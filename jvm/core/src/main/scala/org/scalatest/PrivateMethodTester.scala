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
package org.scalatest

import org.scalactic.Requirements._
import java.lang.reflect.{InvocationTargetException, Method, Modifier}

/**
 * Trait that facilitates the testing of private methods.
 *
 * To test a private method, mix in trait <code>PrivateMethodTester</code> and
 * create a <code>PrivateMethod</code> object, like this: 
 *
 * <pre class="stHighlight">
 * val decorateToStringValue = PrivateMethod[String]('decorateToStringValue)
 * </pre>
 *
 * <p>
 * The type parameter on <code>PrivateMethod</code>, in this case <code>String</code>, is the result type of the private method
 * you wish to invoke. The symbol passed to the <code>PrivateMethod.apply</code> factory method, in this
 * case <code>'decorateToStringValue</code>, is the name of the private method to invoke. To test
 * the private method, use the <code>invokePrivate</code> operator, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * targetObject invokePrivate decorateToStringValue(1)
 * </pre>
 *
 * <p>
 * Here, <code>targetObject</code> is a variable or singleton object name referring to the object whose
 * private method you want to test. You pass the arguments to the private method in the parentheses after
 * the <code>PrivateMethod</code> object.
 * The result type of an <code>invokePrivate</code> operation will be the type parameter of the <code>PrivateMethod</code>
 * object, thus you need not cast the result to use it. In other words, after creating a <code>PrivateMethod</code> object, the
 * syntax to invoke the private method
 * looks like a regular method invocation, but with the dot (<code>.</code>) replaced by <code>invokePrivate</code>.
 * The private method is invoked dynamically via reflection, so if you have a typo in the method name symbol, specify the wrong result type,
 * or pass invalid parameters, the <code>invokePrivate</code> operation will compile, but throw an exception at runtime.
 * </p>
 *
 * <p>
 * One limitation to be aware of is that you can't use <code>PrivateMethodTester</code> to test a private method
 * declared in a trait, because the class the trait gets mixed into will not declare that private method. Only the
 * class generated to hold method implementations for the trait will have that private method. If you want to
 * test a private method declared in a trait, and that method does not use any state of that trait, you can move
 * the private method to a companion object for the trait and test it using <code>PrivateMethodTester</code> that
 * way. If the private trait method you want to test uses the trait's state, your best options are to test it
 * indirectly via a non-private trait method that calls the private method, or make the private method package access
 * and test it directly via regular static method invocations. 
 * </p>
 *
 *
 * <p>
 * Also, if you want to use <code>PrivateMethodTester</code> to invoke a parameterless private method, you'll need to use
 * empty parens. Instead of:
 * </p>
 *
 * <pre class="stHighlight">
 * targetObject invokePrivate privateParameterlessMethod
 * </pre>
 *
 * <p>
 * You'll need to write:
 * </p>
 *
 * <pre class="stHighlight">
 * targetObject invokePrivate privateParameterlessMethod()
 * </pre>
 *
 * @author Bill Venners
 */
trait PrivateMethodTester {

  /**
   * Represent a private method, whose apply method returns an <code>Invocation0</code> object that
   * records the name of the private method to invoke.
   * The type parameter, <code>T</code>, is the return type of the private method.
   *
   * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
   * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
   */
  final class PrivateMethod[T] private (methodName: Symbol) {

    requireNonNull(methodName)

    /**
     * Apply arguments to a private method. This method returns an <code>Invocation</code>
     * object, ready to be passed to an <code>invokePrivate</code> method call.
     * The type parameter, <code>T</code>, is the return type of the private method.
     *
     * @param args zero to many arguments to pass to the private method when invoked
     * @return an <code>Invocation</code> object that can be passed to <code>invokePrivate</code> to invoke
     * the private method
     */
    def apply(args: Any*) = new Invocation[T](methodName, args: _*)
  }

  /**
   * Represent a private method, whose apply method returns an <code>Invocation0</code> object that
   * records the name of the private method to invoke.
   * The type parameter, <code>T</code>, is the return type of the private method.
   *
   * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
   * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
   */
  final class PrivateMethod0[T] private (methodName: Symbol) {

    requireNonNull(methodName)

    /**
     * Apply arguments to a private method. This method returns an <code>Invocation</code>
     * object, ready to be passed to an <code>invokePrivate</code> method call.
     * The type parameter, <code>T</code>, is the return type of the private method.
     *
     * @param args zero to many arguments to pass to the private method when invoked
     * @return an <code>Invocation0</code> object that can be passed to <code>invokePrivate</code> to invoke the private method
     */
    def apply() = new Invocation0[T](methodName)
  }

  /**
   * Represent a private method with 1 argument, whose apply method returns an <code>Invocation1</code> object that
   * records the name of the private method to invoke, and 1 argument type to pass to it when invoked.
   * The type parameter, <code>A1</code> is the argument type, while the <code>T</code>, is the return type of the private method.
   *
   * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
   * @tparam A1 the type of the argument 1
   * @tparam T the return type of the private method
   * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
   */
  final class PrivateMethod1[A1, T] private (methodName: Symbol) {
    /**
     * Apply arguments to a private method. This method returns an <code>Invocation1</code>
     * object, ready to be passed to an <code>invokePrivate</code> method call.
     * The type parameter, <code>A1</code> is the argument type, while the <code>T</code>, is the return type of the private method.
     *
     * @param arg1 argument 1 to pass to the private method when invoked
     * @return an <code>Invocation1</code> object that can be passed to <code>invokePrivate</code> to invoke the private method
     */
    def apply(arg1: A1) = new Invocation1[A1, T](methodName, arg1)
  }

  /**
   * Represent a private method with 2 arguments, whose apply method returns an <code>Invocation2</code> object that
   * records the name of the private method to invoke, and 2 argument types to pass to it when invoked.
   * The type parameter, <code>A1 - A2</code> are the argument types, while the <code>T</code>, is the return type of the private method.
   *
   * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
   * @tparam A1 the type of the argument 1
   * @tparam A2 the type of the argument 2
   * @tparam T the return type of the private method
   * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
   */
  final class PrivateMethod2[A1, A2, T] private (methodName: Symbol) {
    /**
     * Apply arguments to a private method. This method returns an <code>Invocation2</code>
     * object, ready to be passed to an <code>invokePrivate</code> method call.
     * The type parameter, <code>A1 - A2</code> are the argument types, while the <code>T</code>, is the return type of the private method.
     *
     * @param arg1 argument 1 to pass to the private method when invoked
     * @param arg2 argument 2 to pass to the private method when invoked
     * @return an <code>Invocation2</code> object that can be passed to <code>invokePrivate</code> to invoke the private method
     */
    def apply(arg1: A1, arg2: A2) = new Invocation2[A1, A2, T](methodName, arg1, arg2)
  }

  /**
   * Represent a private method with 3 arguments, whose apply method returns an <code>Invocation3</code> object that
   * records the name of the private method to invoke, and 3 argument types to pass to it when invoked.
   * The type parameter, <code>A1 - A3</code> are the argument types, while the <code>T</code>, is the return type of the private method.
   *
   * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
   * @tparam A1 the type of the argument 1
   * @tparam A2 the type of the argument 2
   * @tparam A3 the type of the argument 3
   * @tparam T the return type of the private method
   * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
   */
  final class PrivateMethod3[A1, A2, A3, T] private (methodName: Symbol) {
    /**
     * Apply arguments to a private method. This method returns an <code>Invocation3</code>
     * object, ready to be passed to an <code>invokePrivate</code> method call.
     * The type parameter, <code>A1 - A3</code> are the argument types, while the <code>T</code>, is the return type of the private method.
     *
     * @param arg1 argument 1 to pass to the private method when invoked
     * @param arg2 argument 2 to pass to the private method when invoked
     * @param arg3 argument 3 to pass to the private method when invoked
     * @return an <code>Invocation3</code> object that can be passed to <code>invokePrivate</code> to invoke the private method
     */
    def apply(arg1: A1, arg2: A2, arg3: A3) = new Invocation3[A1, A2, A3, T](methodName, arg1, arg2, arg3)
  }

  /**
   * Represent a private method with 4 arguments, whose apply method returns an <code>Invocation4</code> object that
   * records the name of the private method to invoke, and 4 argument types to pass to it when invoked.
   * The type parameter, <code>A1 - A4</code> are the argument types, while the <code>T</code>, is the return type of the private method.
   *
   * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
   * @tparam A1 the type of the argument 1
   * @tparam A2 the type of the argument 2
   * @tparam A3 the type of the argument 3
   * @tparam A4 the type of the argument 4
   * @tparam T the return type of the private method
   * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
   */
  final class PrivateMethod4[A1, A2, A3, A4, T] private (methodName: Symbol) {
    /**
     * Apply arguments to a private method. This method returns an <code>Invocation4</code>
     * object, ready to be passed to an <code>invokePrivate</code> method call.
     * The type parameter, <code>A1 - A4</code> are the argument types, while the <code>T</code>, is the return type of the private method.
     *
     * @param arg1 argument 1 to pass to the private method when invoked
     * @param arg2 argument 2 to pass to the private method when invoked
     * @param arg3 argument 3 to pass to the private method when invoked
     * @param arg4 argument 4 to pass to the private method when invoked
     * @return an <code>Invocation4</code> object that can be passed to <code>invokePrivate</code> to invoke the private method
     */
    def apply(arg1: A1, arg2: A2, arg3: A3, arg4: A4) = new Invocation4[A1, A2, A3, A4, T](methodName, arg1, arg2, arg3, arg4)
  }

  /**
   * Represent a private method with 5 arguments, whose apply method returns an <code>Invocation5</code> object that
   * records the name of the private method to invoke, and 5 argument types to pass to it when invoked.
   * The type parameter, <code>A1 - A5</code> are the argument types, while the <code>T</code>, is the return type of the private method.
   *
   * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
   * @tparam A1 the type of the argument 1
   * @tparam A2 the type of the argument 2
   * @tparam A3 the type of the argument 3
   * @tparam A4 the type of the argument 4
   * @tparam A5 the type of the argument 5
   * @tparam T the return type of the private method
   * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
   */
  final class PrivateMethod5[A1, A2, A3, A4, A5, T] private (methodName: Symbol) {
    /**
     * Apply arguments to a private method. This method returns an <code>Invocation5</code>
     * object, ready to be passed to an <code>invokePrivate</code> method call.
     * The type parameter, <code>A1 - A5</code> are the argument types, while the <code>T</code>, is the return type of the private method.
     *
     * @param arg1 argument 1 to pass to the private method when invoked
     * @param arg2 argument 2 to pass to the private method when invoked
     * @param arg3 argument 3 to pass to the private method when invoked
     * @param arg4 argument 4 to pass to the private method when invoked
     * @param arg5 argument 5 to pass to the private method when invoked
     * @return an <code>Invocation5</code> object that can be passed to <code>invokePrivate</code> to invoke the private method
     */
    def apply(arg1: A1, arg2: A2, arg3: A3, arg4: A4, arg5: A5) = new Invocation5[A1, A2, A3, A4, A5, T](methodName, arg1, arg2, arg3, arg4, arg5)
  }

  /**
   * Represent a private method with 6 arguments, whose apply method returns an <code>Invocation6</code> object that
   * records the name of the private method to invoke, and 6 argument types to pass to it when invoked.
   * The type parameter, <code>A1 - A6</code> are the argument types, while the <code>T</code>, is the return type of the private method.
   *
   * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
   * @tparam A1 the type of the argument 1
   * @tparam A2 the type of the argument 2
   * @tparam A3 the type of the argument 3
   * @tparam A4 the type of the argument 4
   * @tparam A5 the type of the argument 5
   * @tparam A6 the type of the argument 6
   * @tparam T the return type of the private method
   * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
   */
  final class PrivateMethod6[A1, A2, A3, A4, A5, A6, T] private (methodName: Symbol) {
    /**
     * Apply arguments to a private method. This method returns an <code>Invocation6</code>
     * object, ready to be passed to an <code>invokePrivate</code> method call.
     * The type parameter, <code>A1 - A6</code> are the argument types, while the <code>T</code>, is the return type of the private method.
     *
     * @param arg1 argument 1 to pass to the private method when invoked
     * @param arg2 argument 2 to pass to the private method when invoked
     * @param arg3 argument 3 to pass to the private method when invoked
     * @param arg4 argument 4 to pass to the private method when invoked
     * @param arg5 argument 5 to pass to the private method when invoked
     * @param arg6 argument 6 to pass to the private method when invoked
     * @return an <code>Invocation6</code> object that can be passed to <code>invokePrivate</code> to invoke the private method
     */
    def apply(arg1: A1, arg2: A2, arg3: A3, arg4: A4, arg5: A5, arg6: A6) = new Invocation6[A1, A2, A3, A4, A5, A6, T](methodName, arg1, arg2, arg3, arg4, arg5, arg6)
  }

  /**
   * Represent a private method with 7 arguments, whose apply method returns an <code>Invocation7</code> object that
   * records the name of the private method to invoke, and 7 argument types to pass to it when invoked.
   * The type parameter, <code>A1 - A7</code> are the argument types, while the <code>T</code>, is the return type of the private method.
   *
   * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
   * @tparam A1 the type of the argument 1
   * @tparam A2 the type of the argument 2
   * @tparam A3 the type of the argument 3
   * @tparam A4 the type of the argument 4
   * @tparam A5 the type of the argument 5
   * @tparam A6 the type of the argument 6
   * @tparam A7 the type of the argument 7
   * @tparam T the return type of the private method
   * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
   */
  final class PrivateMethod7[A1, A2, A3, A4, A5, A6, A7, T] private (methodName: Symbol) {
    /**
     * Apply arguments to a private method. This method returns an <code>Invocation7</code>
     * object, ready to be passed to an <code>invokePrivate</code> method call.
     * The type parameter, <code>A1 - A7</code> are the argument types, while the <code>T</code>, is the return type of the private method.
     *
     * @param arg1 argument 1 to pass to the private method when invoked
     * @param arg2 argument 2 to pass to the private method when invoked
     * @param arg3 argument 3 to pass to the private method when invoked
     * @param arg4 argument 4 to pass to the private method when invoked
     * @param arg5 argument 5 to pass to the private method when invoked
     * @param arg6 argument 6 to pass to the private method when invoked
     * @param arg7 argument 7 to pass to the private method when invoked
     * @return an <code>Invocation7</code> object that can be passed to <code>invokePrivate</code> to invoke the private method
     */
    def apply(arg1: A1, arg2: A2, arg3: A3, arg4: A4, arg5: A5, arg6: A6, arg7: A7) = new Invocation7[A1, A2, A3, A4, A5, A6, A7, T](methodName, arg1, arg2, arg3, arg4, arg5, arg6, arg7)
  }

  /**
   * Represent a private method with 8 arguments, whose apply method returns an <code>Invocation8</code> object that
   * records the name of the private method to invoke, and 8 argument types to pass to it when invoked.
   * The type parameter, <code>A1 - A8</code> are the argument types, while the <code>T</code>, is the return type of the private method.
   *
   * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
   * @tparam A1 the type of the argument 1
   * @tparam A2 the type of the argument 2
   * @tparam A3 the type of the argument 3
   * @tparam A4 the type of the argument 4
   * @tparam A5 the type of the argument 5
   * @tparam A6 the type of the argument 6
   * @tparam A7 the type of the argument 7
   * @tparam A8 the type of the argument 8
   * @tparam T the return type of the private method
   * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
   */
  final class PrivateMethod8[A1, A2, A3, A4, A5, A6, A7, A8, T] private (methodName: Symbol) {
    /**
     * Apply arguments to a private method. This method returns an <code>Invocation8</code>
     * object, ready to be passed to an <code>invokePrivate</code> method call.
     * The type parameter, <code>A1 - A8</code> are the argument types, while the <code>T</code>, is the return type of the private method.
     *
     * @param arg1 argument 1 to pass to the private method when invoked
     * @param arg2 argument 2 to pass to the private method when invoked
     * @param arg3 argument 3 to pass to the private method when invoked
     * @param arg4 argument 4 to pass to the private method when invoked
     * @param arg5 argument 5 to pass to the private method when invoked
     * @param arg6 argument 6 to pass to the private method when invoked
     * @param arg7 argument 7 to pass to the private method when invoked
     * @param arg8 argument 8 to pass to the private method when invoked
     * @return an <code>Invocation8</code> object that can be passed to <code>invokePrivate</code> to invoke the private method
     */
    def apply(arg1: A1, arg2: A2, arg3: A3, arg4: A4, arg5: A5, arg6: A6, arg7: A7, arg8: A8) = new Invocation8[A1, A2, A3, A4, A5, A6, A7, A8, T](methodName, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)
  }

  /**
   * Represent a private method with 9 arguments, whose apply method returns an <code>Invocation9</code> object that
   * records the name of the private method to invoke, and 9 argument types to pass to it when invoked.
   * The type parameter, <code>A1 - A9</code> are the argument types, while the <code>T</code>, is the return type of the private method.
   *
   * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
   * @tparam A1 the type of the argument 1
   * @tparam A2 the type of the argument 2
   * @tparam A3 the type of the argument 3
   * @tparam A4 the type of the argument 4
   * @tparam A5 the type of the argument 5
   * @tparam A6 the type of the argument 6
   * @tparam A7 the type of the argument 7
   * @tparam A8 the type of the argument 8
   * @tparam A9 the type of the argument 9
   * @tparam T the return type of the private method
   * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
   */
  final class PrivateMethod9[A1, A2, A3, A4, A5, A6, A7, A8, A9, T] private (methodName: Symbol) {
    /**
     * Apply arguments to a private method. This method returns an <code>Invocation9</code>
     * object, ready to be passed to an <code>invokePrivate</code> method call.
     * The type parameter, <code>A1 - A9</code> are the argument types, while the <code>T</code>, is the return type of the private method.
     *
     * @param arg1 argument 1 to pass to the private method when invoked
     * @param arg2 argument 2 to pass to the private method when invoked
     * @param arg3 argument 3 to pass to the private method when invoked
     * @param arg4 argument 4 to pass to the private method when invoked
     * @param arg5 argument 5 to pass to the private method when invoked
     * @param arg6 argument 6 to pass to the private method when invoked
     * @param arg7 argument 7 to pass to the private method when invoked
     * @param arg8 argument 8 to pass to the private method when invoked
     * @param arg9 argument 9 to pass to the private method when invoked
     * @return an <code>Invocation9</code> object that can be passed to <code>invokePrivate</code> to invoke the private method
     */
    def apply(arg1: A1, arg2: A2, arg3: A3, arg4: A4, arg5: A5, arg6: A6, arg7: A7, arg8: A8, arg9: A9) = new Invocation9[A1, A2, A3, A4, A5, A6, A7, A8, A9, T](methodName, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)
  }

  /**
   * Represent a private method with 10 arguments, whose apply method returns an <code>Invocation10</code> object that
   * records the name of the private method to invoke, and 10 argument types to pass to it when invoked.
   * The type parameter, <code>A1 - A10</code> are the argument types, while the <code>T</code>, is the return type of the private method.
   *
   * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
   * @tparam A1 the type of the argument 1
   * @tparam A2 the type of the argument 2
   * @tparam A3 the type of the argument 3
   * @tparam A4 the type of the argument 4
   * @tparam A5 the type of the argument 5
   * @tparam A6 the type of the argument 6
   * @tparam A7 the type of the argument 7
   * @tparam A8 the type of the argument 8
   * @tparam A9 the type of the argument 9
   * @tparam A10 the type of the argument 10
   * @tparam T the return type of the private method
   * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
   */
  final class PrivateMethod10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, T] private (methodName: Symbol) {
    /**
     * Apply arguments to a private method. This method returns an <code>Invocation10</code>
     * object, ready to be passed to an <code>invokePrivate</code> method call.
     * The type parameter, <code>A1 - A10</code> are the argument types, while the <code>T</code>, is the return type of the private method.
     *
     * @param arg1 argument 1 to pass to the private method when invoked
     * @param arg2 argument 2 to pass to the private method when invoked
     * @param arg3 argument 3 to pass to the private method when invoked
     * @param arg4 argument 4 to pass to the private method when invoked
     * @param arg5 argument 5 to pass to the private method when invoked
     * @param arg6 argument 6 to pass to the private method when invoked
     * @param arg7 argument 7 to pass to the private method when invoked
     * @param arg8 argument 8 to pass to the private method when invoked
     * @param arg9 argument 9 to pass to the private method when invoked
     * @param arg10 argument 10 to pass to the private method when invoked
     * @return an <code>Invocation10</code> object that can be passed to <code>invokePrivate</code> to invoke the private method
     */
    def apply(arg1: A1, arg2: A2, arg3: A3, arg4: A4, arg5: A5, arg6: A6, arg7: A7, arg8: A8, arg9: A9, arg10: A10) = new Invocation10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, T](methodName, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)
  }

  /**
   * Represent a private method with 11 arguments, whose apply method returns an <code>Invocation11</code> object that
   * records the name of the private method to invoke, and 11 argument types to pass to it when invoked.
   * The type parameter, <code>A1 - A11</code> are the argument types, while the <code>T</code>, is the return type of the private method.
   *
   * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
   * @tparam A1 the type of the argument 1
   * @tparam A2 the type of the argument 2
   * @tparam A3 the type of the argument 3
   * @tparam A4 the type of the argument 4
   * @tparam A5 the type of the argument 5
   * @tparam A6 the type of the argument 6
   * @tparam A7 the type of the argument 7
   * @tparam A8 the type of the argument 8
   * @tparam A9 the type of the argument 9
   * @tparam A10 the type of the argument 10
   * @tparam A11 the type of the argument 11
   * @tparam T the return type of the private method
   * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
   */
  final class PrivateMethod11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, T] private (methodName: Symbol) {
    /**
     * Apply arguments to a private method. This method returns an <code>Invocation11</code>
     * object, ready to be passed to an <code>invokePrivate</code> method call.
     * The type parameter, <code>A1 - A11</code> are the argument types, while the <code>T</code>, is the return type of the private method.
     *
     * @param arg1 argument 1 to pass to the private method when invoked
     * @param arg2 argument 2 to pass to the private method when invoked
     * @param arg3 argument 3 to pass to the private method when invoked
     * @param arg4 argument 4 to pass to the private method when invoked
     * @param arg5 argument 5 to pass to the private method when invoked
     * @param arg6 argument 6 to pass to the private method when invoked
     * @param arg7 argument 7 to pass to the private method when invoked
     * @param arg8 argument 8 to pass to the private method when invoked
     * @param arg9 argument 9 to pass to the private method when invoked
     * @param arg10 argument 10 to pass to the private method when invoked
     * @param arg11 argument 11 to pass to the private method when invoked
     * @return an <code>Invocation11</code> object that can be passed to <code>invokePrivate</code> to invoke the private method
     */
    def apply(arg1: A1, arg2: A2, arg3: A3, arg4: A4, arg5: A5, arg6: A6, arg7: A7, arg8: A8, arg9: A9, arg10: A10, arg11: A11) = new Invocation11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, T](methodName, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11)
  }

  /**
   * Represent a private method with 12 arguments, whose apply method returns an <code>Invocation11</code> object that
   * records the name of the private method to invoke, and 12 argument types to pass to it when invoked.
   * The type parameter, <code>A1 - A12</code> are the argument types, while the <code>T</code>, is the return type of the private method.
   *
   * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
   * @tparam A1 the type of the argument 1
   * @tparam A2 the type of the argument 2
   * @tparam A3 the type of the argument 3
   * @tparam A4 the type of the argument 4
   * @tparam A5 the type of the argument 5
   * @tparam A6 the type of the argument 6
   * @tparam A7 the type of the argument 7
   * @tparam A8 the type of the argument 8
   * @tparam A9 the type of the argument 9
   * @tparam A10 the type of the argument 10
   * @tparam A11 the type of the argument 11
   * @tparam A12 the type of the argument 12
   * @tparam T the return type of the private method
   * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
   */
  final class PrivateMethod12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, T] private (methodName: Symbol) {
    /**
     * Apply arguments to a private method. This method returns an <code>Invocation11</code>
     * object, ready to be passed to an <code>invokePrivate</code> method call.
     * The type parameter, <code>A1 - A12</code> are the argument types, while the <code>T</code>, is the return type of the private method.
     *
     * @param arg1 argument 1 to pass to the private method when invoked
     * @param arg2 argument 2 to pass to the private method when invoked
     * @param arg3 argument 3 to pass to the private method when invoked
     * @param arg4 argument 4 to pass to the private method when invoked
     * @param arg5 argument 5 to pass to the private method when invoked
     * @param arg6 argument 6 to pass to the private method when invoked
     * @param arg7 argument 7 to pass to the private method when invoked
     * @param arg8 argument 8 to pass to the private method when invoked
     * @param arg9 argument 9 to pass to the private method when invoked
     * @param arg10 argument 10 to pass to the private method when invoked
     * @param arg11 argument 11 to pass to the private method when invoked
     * @param arg12 argument 12 to pass to the private method when invoked
     * @return an <code>Invocation12</code> object that can be passed to <code>invokePrivate</code> to invoke the private method
     */
    def apply(arg1: A1, arg2: A2, arg3: A3, arg4: A4, arg5: A5, arg6: A6, arg7: A7, arg8: A8, arg9: A9, arg10: A10, arg11: A11, arg12: A12) = 
      new Invocation12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, T](methodName, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12)
  }

  /**
   * Represent a private method with 13 arguments, whose apply method returns an <code>Invocation13</code> object that
   * records the name of the private method to invoke, and 13 argument types to pass to it when invoked.
   * The type parameter, <code>A1 - A13</code> are the argument types, while the <code>T</code>, is the return type of the private method.
   *
   * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
   * @tparam A1 the type of the argument 1
   * @tparam A2 the type of the argument 2
   * @tparam A3 the type of the argument 3
   * @tparam A4 the type of the argument 4
   * @tparam A5 the type of the argument 5
   * @tparam A6 the type of the argument 6
   * @tparam A7 the type of the argument 7
   * @tparam A8 the type of the argument 8
   * @tparam A9 the type of the argument 9
   * @tparam A10 the type of the argument 10
   * @tparam A11 the type of the argument 11
   * @tparam A12 the type of the argument 12
   * @tparam A13 the type of the argument 13
   * @tparam T the return type of the private method
   * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
   */
  final class PrivateMethod13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, T] private (methodName: Symbol) {
    /**
     * Apply arguments to a private method. This method returns an <code>Invocation13</code>
     * object, ready to be passed to an <code>invokePrivate</code> method call.
     * The type parameter, <code>A1 - A13</code> are the argument types, while the <code>T</code>, is the return type of the private method.
     *
     * @param arg1 argument 1 to pass to the private method when invoked
     * @param arg2 argument 2 to pass to the private method when invoked
     * @param arg3 argument 3 to pass to the private method when invoked
     * @param arg4 argument 4 to pass to the private method when invoked
     * @param arg5 argument 5 to pass to the private method when invoked
     * @param arg6 argument 6 to pass to the private method when invoked
     * @param arg7 argument 7 to pass to the private method when invoked
     * @param arg8 argument 8 to pass to the private method when invoked
     * @param arg9 argument 9 to pass to the private method when invoked
     * @param arg10 argument 10 to pass to the private method when invoked
     * @param arg11 argument 11 to pass to the private method when invoked
     * @param arg12 argument 12 to pass to the private method when invoked
     * @param arg13 argument 13 to pass to the private method when invoked
     * @return an <code>Invocation13</code> object that can be passed to <code>invokePrivate</code> to invoke the private method
     */
    def apply(arg1: A1, arg2: A2, arg3: A3, arg4: A4, arg5: A5, arg6: A6, arg7: A7, arg8: A8, arg9: A9, arg10: A10, arg11: A11, arg12: A12, 
              arg13: A13) = 
      new Invocation13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, T](
        methodName, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13)
  }

  /**
   * Represent a private method with 14 arguments, whose apply method returns an <code>Invocation14</code> object that
   * records the name of the private method to invoke, and 14 argument types to pass to it when invoked.
   * The type parameter, <code>A1 - A14</code> are the argument types, while the <code>T</code>, is the return type of the private method.
   *
   * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
   * @tparam A1 the type of the argument 1
   * @tparam A2 the type of the argument 2
   * @tparam A3 the type of the argument 3
   * @tparam A4 the type of the argument 4
   * @tparam A5 the type of the argument 5
   * @tparam A6 the type of the argument 6
   * @tparam A7 the type of the argument 7
   * @tparam A8 the type of the argument 8
   * @tparam A9 the type of the argument 9
   * @tparam A10 the type of the argument 10
   * @tparam A11 the type of the argument 11
   * @tparam A12 the type of the argument 12
   * @tparam A13 the type of the argument 13
   * @tparam A14 the type of the argument 14
   * @tparam T the return type of the private method
   * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
   */
  final class PrivateMethod14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, T] private (methodName: Symbol) {
    /**
     * Apply arguments to a private method. This method returns an <code>Invocation14</code>
     * object, ready to be passed to an <code>invokePrivate</code> method call.
     * The type parameter, <code>A1 - A14</code> are the argument types, while the <code>T</code>, is the return type of the private method.
     *
     * @param arg1 argument 1 to pass to the private method when invoked
     * @param arg2 argument 2 to pass to the private method when invoked
     * @param arg3 argument 3 to pass to the private method when invoked
     * @param arg4 argument 4 to pass to the private method when invoked
     * @param arg5 argument 5 to pass to the private method when invoked
     * @param arg6 argument 6 to pass to the private method when invoked
     * @param arg7 argument 7 to pass to the private method when invoked
     * @param arg8 argument 8 to pass to the private method when invoked
     * @param arg9 argument 9 to pass to the private method when invoked
     * @param arg10 argument 10 to pass to the private method when invoked
     * @param arg11 argument 11 to pass to the private method when invoked
     * @param arg12 argument 12 to pass to the private method when invoked
     * @param arg13 argument 13 to pass to the private method when invoked
     * @param arg14 argument 14 to pass to the private method when invoked
     * @return an <code>Invocation14</code> object that can be passed to <code>invokePrivate</code> to invoke the private method
     */
    def apply(arg1: A1, arg2: A2, arg3: A3, arg4: A4, arg5: A5, arg6: A6, arg7: A7, arg8: A8, arg9: A9, arg10: A10, arg11: A11, arg12: A12, 
              arg13: A13, arg14: A14) = 
      new Invocation14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, T](
        methodName, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14)
  }

  /**
   * Represent a private method with 15 arguments, whose apply method returns an <code>Invocation15</code> object that
   * records the name of the private method to invoke, and 15 argument types to pass to it when invoked.
   * The type parameter, <code>A1 - A15</code> are the argument types, while the <code>T</code>, is the return type of the private method.
   *
   * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
   * @tparam A1 the type of the argument 1
   * @tparam A2 the type of the argument 2
   * @tparam A3 the type of the argument 3
   * @tparam A4 the type of the argument 4
   * @tparam A5 the type of the argument 5
   * @tparam A6 the type of the argument 6
   * @tparam A7 the type of the argument 7
   * @tparam A8 the type of the argument 8
   * @tparam A9 the type of the argument 9
   * @tparam A10 the type of the argument 10
   * @tparam A11 the type of the argument 11
   * @tparam A12 the type of the argument 12
   * @tparam A13 the type of the argument 13
   * @tparam A14 the type of the argument 14
   * @tparam A15 the type of the argument 15
   * @tparam T the return type of the private method
   * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
   */
  final class PrivateMethod15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, T] private (methodName: Symbol) {
    /**
     * Apply arguments to a private method. This method returns an <code>Invocation15</code>
     * object, ready to be passed to an <code>invokePrivate</code> method call.
     * The type parameter, <code>A1 - A15</code> are the argument types, while the <code>T</code>, is the return type of the private method.
     *
     * @param arg1 argument 1 to pass to the private method when invoked
     * @param arg2 argument 2 to pass to the private method when invoked
     * @param arg3 argument 3 to pass to the private method when invoked
     * @param arg4 argument 4 to pass to the private method when invoked
     * @param arg5 argument 5 to pass to the private method when invoked
     * @param arg6 argument 6 to pass to the private method when invoked
     * @param arg7 argument 7 to pass to the private method when invoked
     * @param arg8 argument 8 to pass to the private method when invoked
     * @param arg9 argument 9 to pass to the private method when invoked
     * @param arg10 argument 10 to pass to the private method when invoked
     * @param arg11 argument 11 to pass to the private method when invoked
     * @param arg12 argument 12 to pass to the private method when invoked
     * @param arg13 argument 13 to pass to the private method when invoked
     * @param arg14 argument 14 to pass to the private method when invoked
     * @param arg15 argument 15 to pass to the private method when invoked
     * @return an <code>Invocation15</code> object that can be passed to <code>invokePrivate</code> to invoke the private method
     */
    def apply(arg1: A1, arg2: A2, arg3: A3, arg4: A4, arg5: A5, arg6: A6, arg7: A7, arg8: A8, arg9: A9, arg10: A10, arg11: A11, arg12: A12, 
              arg13: A13, arg14: A14, arg15: A15) = 
      new Invocation15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, T](
        methodName, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15)
  }

  /**
   * Represent a private method with 16 arguments, whose apply method returns an <code>Invocation16</code> object that
   * records the name of the private method to invoke, and 16 argument types to pass to it when invoked.
   * The type parameter, <code>A1 - A16</code> are the argument types, while the <code>T</code>, is the return type of the private method.
   *
   * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
   * @tparam A1 the type of the argument 1
   * @tparam A2 the type of the argument 2
   * @tparam A3 the type of the argument 3
   * @tparam A4 the type of the argument 4
   * @tparam A5 the type of the argument 5
   * @tparam A6 the type of the argument 6
   * @tparam A7 the type of the argument 7
   * @tparam A8 the type of the argument 8
   * @tparam A9 the type of the argument 9
   * @tparam A10 the type of the argument 10
   * @tparam A11 the type of the argument 11
   * @tparam A12 the type of the argument 12
   * @tparam A13 the type of the argument 13
   * @tparam A14 the type of the argument 14
   * @tparam A15 the type of the argument 15
   * @tparam A16 the type of the argument 16
   * @tparam T the return type of the private method
   * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
   */
  final class PrivateMethod16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, T] private (methodName: Symbol) {
    /**
     * Apply arguments to a private method. This method returns an <code>Invocation16</code>
     * object, ready to be passed to an <code>invokePrivate</code> method call.
     * The type parameter, <code>A1 - A16</code> are the argument types, while the <code>T</code>, is the return type of the private method.
     *
     * @param arg1 argument 1 to pass to the private method when invoked
     * @param arg2 argument 2 to pass to the private method when invoked
     * @param arg3 argument 3 to pass to the private method when invoked
     * @param arg4 argument 4 to pass to the private method when invoked
     * @param arg5 argument 5 to pass to the private method when invoked
     * @param arg6 argument 6 to pass to the private method when invoked
     * @param arg7 argument 7 to pass to the private method when invoked
     * @param arg8 argument 8 to pass to the private method when invoked
     * @param arg9 argument 9 to pass to the private method when invoked
     * @param arg10 argument 10 to pass to the private method when invoked
     * @param arg11 argument 11 to pass to the private method when invoked
     * @param arg12 argument 12 to pass to the private method when invoked
     * @param arg13 argument 13 to pass to the private method when invoked
     * @param arg14 argument 14 to pass to the private method when invoked
     * @param arg15 argument 15 to pass to the private method when invoked
     * @param arg16 argument 16 to pass to the private method when invoked
     * @return an <code>Invocation16</code> object that can be passed to <code>invokePrivate</code> to invoke the private method
     */
    def apply(arg1: A1, arg2: A2, arg3: A3, arg4: A4, arg5: A5, arg6: A6, arg7: A7, arg8: A8, arg9: A9, arg10: A10, arg11: A11, arg12: A12, 
              arg13: A13, arg14: A14, arg15: A15, arg16: A16) = 
      new Invocation16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, T](
        methodName, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16)
  }

  /**
   * Represent a private method with 17 arguments, whose apply method returns an <code>Invocation17</code> object that
   * records the name of the private method to invoke, and 17 argument types to pass to it when invoked.
   * The type parameter, <code>A1 - A17</code> are the argument types, while the <code>T</code>, is the return type of the private method.
   *
   * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
   * @tparam A1 the type of the argument 1
   * @tparam A2 the type of the argument 2
   * @tparam A3 the type of the argument 3
   * @tparam A4 the type of the argument 4
   * @tparam A5 the type of the argument 5
   * @tparam A6 the type of the argument 6
   * @tparam A7 the type of the argument 7
   * @tparam A8 the type of the argument 8
   * @tparam A9 the type of the argument 9
   * @tparam A10 the type of the argument 10
   * @tparam A11 the type of the argument 11
   * @tparam A12 the type of the argument 12
   * @tparam A13 the type of the argument 13
   * @tparam A14 the type of the argument 14
   * @tparam A15 the type of the argument 15
   * @tparam A16 the type of the argument 16
   * @tparam A17 the type of the argument 17
   * @tparam T the return type of the private method
   * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
   */
  final class PrivateMethod17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, T] private (methodName: Symbol) {
    /**
     * Apply arguments to a private method. This method returns an <code>Invocation17</code>
     * object, ready to be passed to an <code>invokePrivate</code> method call.
     * The type parameter, <code>A1 - A17</code> are the argument types, while the <code>T</code>, is the return type of the private method.
     *
     * @param arg1 argument 1 to pass to the private method when invoked
     * @param arg2 argument 2 to pass to the private method when invoked
     * @param arg3 argument 3 to pass to the private method when invoked
     * @param arg4 argument 4 to pass to the private method when invoked
     * @param arg5 argument 5 to pass to the private method when invoked
     * @param arg6 argument 6 to pass to the private method when invoked
     * @param arg7 argument 7 to pass to the private method when invoked
     * @param arg8 argument 8 to pass to the private method when invoked
     * @param arg9 argument 9 to pass to the private method when invoked
     * @param arg10 argument 10 to pass to the private method when invoked
     * @param arg11 argument 11 to pass to the private method when invoked
     * @param arg12 argument 12 to pass to the private method when invoked
     * @param arg13 argument 13 to pass to the private method when invoked
     * @param arg14 argument 14 to pass to the private method when invoked
     * @param arg15 argument 15 to pass to the private method when invoked
     * @param arg16 argument 16 to pass to the private method when invoked
     * @param arg17 argument 17 to pass to the private method when invoked
     * @return an <code>Invocation17</code> object that can be passed to <code>invokePrivate</code> to invoke the private method
     */
    def apply(arg1: A1, arg2: A2, arg3: A3, arg4: A4, arg5: A5, arg6: A6, arg7: A7, arg8: A8, arg9: A9, arg10: A10, arg11: A11, arg12: A12, 
              arg13: A13, arg14: A14, arg15: A15, arg16: A16, arg17: A17) = 
      new Invocation17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, T](
        methodName, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17)
  }

  /**
   * Represent a private method with 18 arguments, whose apply method returns an <code>Invocation18</code> object that
   * records the name of the private method to invoke, and 18 argument types to pass to it when invoked.
   * The type parameter, <code>A1 - A18</code> are the argument types, while the <code>T</code>, is the return type of the private method.
   *
   * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
   * @tparam A1 the type of the argument 1
   * @tparam A2 the type of the argument 2
   * @tparam A3 the type of the argument 3
   * @tparam A4 the type of the argument 4
   * @tparam A5 the type of the argument 5
   * @tparam A6 the type of the argument 6
   * @tparam A7 the type of the argument 7
   * @tparam A8 the type of the argument 8
   * @tparam A9 the type of the argument 9
   * @tparam A10 the type of the argument 10
   * @tparam A11 the type of the argument 11
   * @tparam A12 the type of the argument 12
   * @tparam A13 the type of the argument 13
   * @tparam A14 the type of the argument 14
   * @tparam A15 the type of the argument 15
   * @tparam A16 the type of the argument 16
   * @tparam A17 the type of the argument 17
   * @tparam A18 the type of the argument 18
   * @tparam T the return type of the private method
   * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
   */
  final class PrivateMethod18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, T] private (methodName: Symbol) {
    /**
     * Apply arguments to a private method. This method returns an <code>Invocation18</code>
     * object, ready to be passed to an <code>invokePrivate</code> method call.
     * The type parameter, <code>A1 - A18</code> are the argument types, while the <code>T</code>, is the return type of the private method.
     *
     * @param arg1 argument 1 to pass to the private method when invoked
     * @param arg2 argument 2 to pass to the private method when invoked
     * @param arg3 argument 3 to pass to the private method when invoked
     * @param arg4 argument 4 to pass to the private method when invoked
     * @param arg5 argument 5 to pass to the private method when invoked
     * @param arg6 argument 6 to pass to the private method when invoked
     * @param arg7 argument 7 to pass to the private method when invoked
     * @param arg8 argument 8 to pass to the private method when invoked
     * @param arg9 argument 9 to pass to the private method when invoked
     * @param arg10 argument 10 to pass to the private method when invoked
     * @param arg11 argument 11 to pass to the private method when invoked
     * @param arg12 argument 12 to pass to the private method when invoked
     * @param arg13 argument 13 to pass to the private method when invoked
     * @param arg14 argument 14 to pass to the private method when invoked
     * @param arg15 argument 15 to pass to the private method when invoked
     * @param arg16 argument 16 to pass to the private method when invoked
     * @param arg17 argument 17 to pass to the private method when invoked
     * @param arg18 argument 18 to pass to the private method when invoked
     * @return an <code>Invocation18</code> object that can be passed to <code>invokePrivate</code> to invoke the private method
     */
    def apply(arg1: A1, arg2: A2, arg3: A3, arg4: A4, arg5: A5, arg6: A6, arg7: A7, arg8: A8, arg9: A9, arg10: A10, arg11: A11, arg12: A12, 
              arg13: A13, arg14: A14, arg15: A15, arg16: A16, arg17: A17, arg18: A18) = 
      new Invocation18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, T](
        methodName, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18)
  }

  /**
   * Represent a private method with 19 arguments, whose apply method returns an <code>Invocation19</code> object that
   * records the name of the private method to invoke, and 19 argument types to pass to it when invoked.
   * The type parameter, <code>A1 - A19</code> are the argument types, while the <code>T</code>, is the return type of the private method.
   *
   * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
   * @tparam A1 the type of the argument 1
   * @tparam A2 the type of the argument 2
   * @tparam A3 the type of the argument 3
   * @tparam A4 the type of the argument 4
   * @tparam A5 the type of the argument 5
   * @tparam A6 the type of the argument 6
   * @tparam A7 the type of the argument 7
   * @tparam A8 the type of the argument 8
   * @tparam A9 the type of the argument 9
   * @tparam A10 the type of the argument 10
   * @tparam A11 the type of the argument 11
   * @tparam A12 the type of the argument 12
   * @tparam A13 the type of the argument 13
   * @tparam A14 the type of the argument 14
   * @tparam A15 the type of the argument 15
   * @tparam A16 the type of the argument 16
   * @tparam A17 the type of the argument 17
   * @tparam A18 the type of the argument 18
   * @tparam A19 the type of the argument 19
   * @tparam T the return type of the private method
   * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
   */
  final class PrivateMethod19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, T] private (methodName: Symbol) {
    /**
     * Apply arguments to a private method. This method returns an <code>Invocation19</code>
     * object, ready to be passed to an <code>invokePrivate</code> method call.
     * The type parameter, <code>A1 - A19</code> are the argument types, while the <code>T</code>, is the return type of the private method.
     *
     * @param arg1 argument 1 to pass to the private method when invoked
     * @param arg2 argument 2 to pass to the private method when invoked
     * @param arg3 argument 3 to pass to the private method when invoked
     * @param arg4 argument 4 to pass to the private method when invoked
     * @param arg5 argument 5 to pass to the private method when invoked
     * @param arg6 argument 6 to pass to the private method when invoked
     * @param arg7 argument 7 to pass to the private method when invoked
     * @param arg8 argument 8 to pass to the private method when invoked
     * @param arg9 argument 9 to pass to the private method when invoked
     * @param arg10 argument 10 to pass to the private method when invoked
     * @param arg11 argument 11 to pass to the private method when invoked
     * @param arg12 argument 12 to pass to the private method when invoked
     * @param arg13 argument 13 to pass to the private method when invoked
     * @param arg14 argument 14 to pass to the private method when invoked
     * @param arg15 argument 15 to pass to the private method when invoked
     * @param arg16 argument 16 to pass to the private method when invoked
     * @param arg17 argument 17 to pass to the private method when invoked
     * @param arg18 argument 18 to pass to the private method when invoked
     * @param arg19 argument 19 to pass to the private method when invoked
     * @return an <code>Invocation19</code> object that can be passed to <code>invokePrivate</code> to invoke the private method
     */
    def apply(arg1: A1, arg2: A2, arg3: A3, arg4: A4, arg5: A5, arg6: A6, arg7: A7, arg8: A8, arg9: A9, arg10: A10, arg11: A11, arg12: A12, 
              arg13: A13, arg14: A14, arg15: A15, arg16: A16, arg17: A17, arg18: A18, arg19: A19) = 
      new Invocation19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, T](
        methodName, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19)
  }

  /**
   * Represent a private method with 20 arguments, whose apply method returns an <code>Invocation20</code> object that
   * records the name of the private method to invoke, and 20 argument types to pass to it when invoked.
   * The type parameter, <code>A1 - A20</code> are the argument types, while the <code>T</code>, is the return type of the private method.
   *
   * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
   * @tparam A1 the type of the argument 1
   * @tparam A2 the type of the argument 2
   * @tparam A3 the type of the argument 3
   * @tparam A4 the type of the argument 4
   * @tparam A5 the type of the argument 5
   * @tparam A6 the type of the argument 6
   * @tparam A7 the type of the argument 7
   * @tparam A8 the type of the argument 8
   * @tparam A9 the type of the argument 9
   * @tparam A10 the type of the argument 10
   * @tparam A11 the type of the argument 11
   * @tparam A12 the type of the argument 12
   * @tparam A13 the type of the argument 13
   * @tparam A14 the type of the argument 14
   * @tparam A15 the type of the argument 15
   * @tparam A16 the type of the argument 16
   * @tparam A17 the type of the argument 17
   * @tparam A18 the type of the argument 18
   * @tparam A19 the type of the argument 19
   * @tparam A20 the type of the argument 20
   * @tparam T the return type of the private method
   * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
   */
  final class PrivateMethod20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, T] private (methodName: Symbol) {
    /**
     * Apply arguments to a private method. This method returns an <code>Invocation20</code>
     * object, ready to be passed to an <code>invokePrivate</code> method call.
     * The type parameter, <code>A1 - A20</code> are the argument types, while the <code>T</code>, is the return type of the private method.
     *
     * @param arg1 argument 1 to pass to the private method when invoked
     * @param arg2 argument 2 to pass to the private method when invoked
     * @param arg3 argument 3 to pass to the private method when invoked
     * @param arg4 argument 4 to pass to the private method when invoked
     * @param arg5 argument 5 to pass to the private method when invoked
     * @param arg6 argument 6 to pass to the private method when invoked
     * @param arg7 argument 7 to pass to the private method when invoked
     * @param arg8 argument 8 to pass to the private method when invoked
     * @param arg9 argument 9 to pass to the private method when invoked
     * @param arg10 argument 10 to pass to the private method when invoked
     * @param arg11 argument 11 to pass to the private method when invoked
     * @param arg12 argument 12 to pass to the private method when invoked
     * @param arg13 argument 13 to pass to the private method when invoked
     * @param arg14 argument 14 to pass to the private method when invoked
     * @param arg15 argument 15 to pass to the private method when invoked
     * @param arg16 argument 16 to pass to the private method when invoked
     * @param arg17 argument 17 to pass to the private method when invoked
     * @param arg18 argument 18 to pass to the private method when invoked
     * @param arg19 argument 19 to pass to the private method when invoked
     * @param arg20 argument 20 to pass to the private method when invoked
     * @return an <code>Invocation20</code> object that can be passed to <code>invokePrivate</code> to invoke the private method
     */
    def apply(arg1: A1, arg2: A2, arg3: A3, arg4: A4, arg5: A5, arg6: A6, arg7: A7, arg8: A8, arg9: A9, arg10: A10, arg11: A11, arg12: A12, 
              arg13: A13, arg14: A14, arg15: A15, arg16: A16, arg17: A17, arg18: A18, arg19: A19, arg20: A20) = 
      new Invocation20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, T](
        methodName, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20)
  }

  /**
   * Represent a private method with 21 arguments, whose apply method returns an <code>Invocation21</code> object that
   * records the name of the private method to invoke, and 21 argument types to pass to it when invoked.
   * The type parameter, <code>A1 - A21</code> are the argument types, while the <code>T</code>, is the return type of the private method.
   *
   * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
   * @tparam A1 the type of the argument 1
   * @tparam A2 the type of the argument 2
   * @tparam A3 the type of the argument 3
   * @tparam A4 the type of the argument 4
   * @tparam A5 the type of the argument 5
   * @tparam A6 the type of the argument 6
   * @tparam A7 the type of the argument 7
   * @tparam A8 the type of the argument 8
   * @tparam A9 the type of the argument 9
   * @tparam A10 the type of the argument 10
   * @tparam A11 the type of the argument 11
   * @tparam A12 the type of the argument 12
   * @tparam A13 the type of the argument 13
   * @tparam A14 the type of the argument 14
   * @tparam A15 the type of the argument 15
   * @tparam A16 the type of the argument 16
   * @tparam A17 the type of the argument 17
   * @tparam A18 the type of the argument 18
   * @tparam A19 the type of the argument 19
   * @tparam A20 the type of the argument 20
   * @tparam A21 the type of the argument 21
   * @tparam T the return type of the private method
   * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
   */
  final class PrivateMethod21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, T] private (methodName: Symbol) {
    /**
     * Apply arguments to a private method. This method returns an <code>Invocation21</code>
     * object, ready to be passed to an <code>invokePrivate</code> method call.
     * The type parameter, <code>A1 - A21</code> are the argument types, while the <code>T</code>, is the return type of the private method.
     *
     * @param arg1 argument 1 to pass to the private method when invoked
     * @param arg2 argument 2 to pass to the private method when invoked
     * @param arg3 argument 3 to pass to the private method when invoked
     * @param arg4 argument 4 to pass to the private method when invoked
     * @param arg5 argument 5 to pass to the private method when invoked
     * @param arg6 argument 6 to pass to the private method when invoked
     * @param arg7 argument 7 to pass to the private method when invoked
     * @param arg8 argument 8 to pass to the private method when invoked
     * @param arg9 argument 9 to pass to the private method when invoked
     * @param arg10 argument 10 to pass to the private method when invoked
     * @param arg11 argument 11 to pass to the private method when invoked
     * @param arg12 argument 12 to pass to the private method when invoked
     * @param arg13 argument 13 to pass to the private method when invoked
     * @param arg14 argument 14 to pass to the private method when invoked
     * @param arg15 argument 15 to pass to the private method when invoked
     * @param arg16 argument 16 to pass to the private method when invoked
     * @param arg17 argument 17 to pass to the private method when invoked
     * @param arg18 argument 18 to pass to the private method when invoked
     * @param arg19 argument 19 to pass to the private method when invoked
     * @param arg20 argument 20 to pass to the private method when invoked
     * @param arg21 argument 21 to pass to the private method when invoked
     * @return an <code>Invocation21</code> object that can be passed to <code>invokePrivate</code> to invoke the private method
     */
    def apply(arg1: A1, arg2: A2, arg3: A3, arg4: A4, arg5: A5, arg6: A6, arg7: A7, arg8: A8, arg9: A9, arg10: A10, arg11: A11, arg12: A12, 
              arg13: A13, arg14: A14, arg15: A15, arg16: A16, arg17: A17, arg18: A18, arg19: A19, arg20: A20, arg21: A21) = 
      new Invocation21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, T](
        methodName, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21)
  }

  /**
   * Contains a factory method for instantiating <code>PrivateMethod</code> objects.
   */
  object PrivateMethod {

    /**
     * Construct a new <code>PrivateMethod</code> object with passed <code>methodName</code> symbol.
     * The type parameter, <code>T</code>, is the return type of the private method.
     *
     * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
     * @tparam T the return type of the private method
     * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
     */
    def apply[T](methodName: Symbol) = new PrivateMethod[T](methodName)
  }

  /**
   * Contains a factory method for instantiating <code>PrivateMethod1</code> objects.
   */
  object PrivateMethod1 {
    /**
     * Construct a new <code>PrivateMethod1</code> object with passed <code>methodName</code> symbol.
     * The type parameter, <code>A1</code> is the type of the argument and <code>T</code>, is the return type of the private method.
     *
     * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
     * @tparam A1 the type of the argument 1
     * @tparam T the return type of the private method
     * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
     */
    def apply[A1, T](methodName: Symbol) = new PrivateMethod1[A1, T](methodName)
  }

  /**
   * Contains a factory method for instantiating <code>PrivateMethod2</code> objects.
   */
  object PrivateMethod2 {
    /**
     * Construct a new <code>PrivateMethod2</code> object with passed <code>methodName</code> symbol.
     * The type parameter, <code>A1 - A2</code> are the types of the arguments and <code>T</code>, is the return type of the private method.
     *
     * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
     * @tparam A1 the type of the argument 1
     * @tparam A2 the type of the argument 2
     * @tparam T the return type of the private method
     * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
     */
    def apply[A1, A2, T](methodName: Symbol) = new PrivateMethod2[A1, A2, T](methodName)
  }

  /**
   * Contains a factory method for instantiating <code>PrivateMethod3</code> objects.
   */
  object PrivateMethod3 {
    /**
     * Construct a new <code>PrivateMethod3</code> object with passed <code>methodName</code> symbol.
     * The type parameter, <code>A1 - A3</code> are the types of the arguments and <code>T</code>, is the return type of the private method.
     *
     * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
     * @tparam A1 the type of the argument 1
     * @tparam A2 the type of the argument 2
     * @tparam A3 the type of the argument 3
     * @tparam T the return type of the private method
     * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
     */
    def apply[A1, A2, A3, T](methodName: Symbol) = new PrivateMethod3[A1, A2, A3, T](methodName)
  }

  /**
   * Contains a factory method for instantiating <code>PrivateMethod4</code> objects.
   */
  object PrivateMethod4 {
    /**
     * Construct a new <code>PrivateMethod4</code> object with passed <code>methodName</code> symbol.
     * The type parameter, <code>A1 - A4</code> are the types of the arguments and <code>T</code>, is the return type of the private method.
     *
     * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
     * @tparam A1 the type of the argument 1
     * @tparam A2 the type of the argument 2
     * @tparam A3 the type of the argument 3
     * @tparam A4 the type of the argument 4
     * @tparam T the return type of the private method
     * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
     */
    def apply[A1, A2, A3, A4, T](methodName: Symbol) = new PrivateMethod4[A1, A2, A3, A4, T](methodName)
  }

  /**
   * Contains a factory method for instantiating <code>PrivateMethod5</code> objects.
   */
  object PrivateMethod5 {
    /**
     * Construct a new <code>PrivateMethod5</code> object with passed <code>methodName</code> symbol.
     * The type parameter, <code>A1 - A5</code> are the types of the arguments and <code>T</code>, is the return type of the private method.
     *
     * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
     * @tparam A1 the type of the argument 1
     * @tparam A2 the type of the argument 2
     * @tparam A3 the type of the argument 3
     * @tparam A4 the type of the argument 4
     * @tparam A5 the type of the argument 5
     * @tparam T the return type of the private method
     * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
     */
    def apply[A1, A2, A3, A4, A5, T](methodName: Symbol) = new PrivateMethod5[A1, A2, A3, A4, A5, T](methodName)
  }

  /**
   * Contains a factory method for instantiating <code>PrivateMethod6</code> objects.
   */
  object PrivateMethod6 {
    /**
     * Construct a new <code>PrivateMethod6</code> object with passed <code>methodName</code> symbol.
     * The type parameter, <code>A1 - A6</code> are the types of the arguments and <code>T</code>, is the return type of the private method.
     *
     * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
     * @tparam A1 the type of the argument 1
     * @tparam A2 the type of the argument 2
     * @tparam A3 the type of the argument 3
     * @tparam A4 the type of the argument 4
     * @tparam A5 the type of the argument 5
     * @tparam A6 the type of the argument 6
     * @tparam T the return type of the private method
     * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
     */
    def apply[A1, A2, A3, A4, A5, A6, T](methodName: Symbol) = new PrivateMethod6[A1, A2, A3, A4, A5, A6, T](methodName)
  }

  /**
   * Contains a factory method for instantiating <code>PrivateMethod7</code> objects.
   */
  object PrivateMethod7 {
    /**
     * Construct a new <code>PrivateMethod7</code> object with passed <code>methodName</code> symbol.
     * The type parameter, <code>A1 - A7</code> are the types of the arguments and <code>T</code>, is the return type of the private method.
     *
     * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
     * @tparam A1 the type of the argument 1
     * @tparam A2 the type of the argument 2
     * @tparam A3 the type of the argument 3
     * @tparam A4 the type of the argument 4
     * @tparam A5 the type of the argument 5
     * @tparam A6 the type of the argument 6
     * @tparam A7 the type of the argument 7
     * @tparam T the return type of the private method
     * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
     */
    def apply[A1, A2, A3, A4, A5, A6, A7, T](methodName: Symbol) = new PrivateMethod7[A1, A2, A3, A4, A5, A6, A7, T](methodName)
  }

  /**
   * Contains a factory method for instantiating <code>PrivateMethod8</code> objects.
   */
  object PrivateMethod8 {
    /**
     * Construct a new <code>PrivateMethod8</code> object with passed <code>methodName</code> symbol.
     * The type parameter, <code>A1 - A8</code> are the types of the arguments and <code>T</code>, is the return type of the private method.
     *
     * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
     * @tparam A1 the type of the argument 1
     * @tparam A2 the type of the argument 2
     * @tparam A3 the type of the argument 3
     * @tparam A4 the type of the argument 4
     * @tparam A5 the type of the argument 5
     * @tparam A6 the type of the argument 6
     * @tparam A7 the type of the argument 7
     * @tparam A8 the type of the argument 8
     * @tparam T the return type of the private method
     * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
     */
    def apply[A1, A2, A3, A4, A5, A6, A7, A8, T](methodName: Symbol) = new PrivateMethod8[A1, A2, A3, A4, A5, A6, A7, A8, T](methodName)
  }

  /**
   * Contains a factory method for instantiating <code>PrivateMethod9</code> objects.
   */
  object PrivateMethod9 {
    /**
     * Construct a new <code>PrivateMethod9</code> object with passed <code>methodName</code> symbol.
     * The type parameter, <code>A1 - A9</code> are the types of the arguments and <code>T</code>, is the return type of the private method.
     *
     * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
     * @tparam A1 the type of the argument 1
     * @tparam A2 the type of the argument 2
     * @tparam A3 the type of the argument 3
     * @tparam A4 the type of the argument 4
     * @tparam A5 the type of the argument 5
     * @tparam A6 the type of the argument 6
     * @tparam A7 the type of the argument 7
     * @tparam A8 the type of the argument 8
     * @tparam A9 the type of the argument 9
     * @tparam T the return type of the private method
     * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
     */
    def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, T](methodName: Symbol) = new PrivateMethod9[A1, A2, A3, A4, A5, A6, A7, A8, A9, T](methodName)
  }

  /**
   * Contains a factory method for instantiating <code>PrivateMethod10</code> objects.
   */
  object PrivateMethod10 {
    /**
     * Construct a new <code>PrivateMethod10</code> object with passed <code>methodName</code> symbol.
     * The type parameter, <code>A1 - A10</code> are the types of the arguments and <code>T</code>, is the return type of the private method.
     *
     * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
     * @tparam A1 the type of the argument 1
     * @tparam A2 the type of the argument 2
     * @tparam A3 the type of the argument 3
     * @tparam A4 the type of the argument 4
     * @tparam A5 the type of the argument 5
     * @tparam A6 the type of the argument 6
     * @tparam A7 the type of the argument 7
     * @tparam A8 the type of the argument 8
     * @tparam A9 the type of the argument 9
     * @tparam A10 the type of the argument 10
     * @tparam T the return type of the private method
     * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
     */
    def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, T](methodName: Symbol) = new PrivateMethod10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, T](methodName)
  }

  /**
   * Contains a factory method for instantiating <code>PrivateMethod11</code> objects.
   */
  object PrivateMethod11 {
    /**
     * Construct a new <code>PrivateMethod11</code> object with passed <code>methodName</code> symbol.
     * The type parameter, <code>A1 - A11</code> are the types of the arguments and <code>T</code>, is the return type of the private method.
     *
     * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
     * @tparam A1 the type of the argument 1
     * @tparam A2 the type of the argument 2
     * @tparam A3 the type of the argument 3
     * @tparam A4 the type of the argument 4
     * @tparam A5 the type of the argument 5
     * @tparam A6 the type of the argument 6
     * @tparam A7 the type of the argument 7
     * @tparam A8 the type of the argument 8
     * @tparam A9 the type of the argument 9
     * @tparam A10 the type of the argument 10
     * @tparam A11 the type of the argument 11
     * @tparam T the return type of the private method
     * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
     */
    def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, T](methodName: Symbol) = new PrivateMethod11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, T](methodName)
  }

  /**
   * Contains a factory method for instantiating <code>PrivateMethod12</code> objects.
   */
  object PrivateMethod12 {
    /**
     * Construct a new <code>PrivateMethod12</code> object with passed <code>methodName</code> symbol.
     * The type parameter, <code>A1 - A12</code> are the types of the arguments and <code>T</code>, is the return type of the private method.
     *
     * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
     * @tparam A1 the type of the argument 1
     * @tparam A2 the type of the argument 2
     * @tparam A3 the type of the argument 3
     * @tparam A4 the type of the argument 4
     * @tparam A5 the type of the argument 5
     * @tparam A6 the type of the argument 6
     * @tparam A7 the type of the argument 7
     * @tparam A8 the type of the argument 8
     * @tparam A9 the type of the argument 9
     * @tparam A10 the type of the argument 10
     * @tparam A11 the type of the argument 11
     * @tparam A12 the type of the argument 12
     * @tparam T the return type of the private method
     * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
     */
    def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, T](methodName: Symbol) = new PrivateMethod12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, T](methodName)
  }

  /**
   * Contains a factory method for instantiating <code>PrivateMethod13</code> objects.
   */
  object PrivateMethod13 {
    /**
     * Construct a new <code>PrivateMethod13</code> object with passed <code>methodName</code> symbol.
     * The type parameter, <code>A1 - A13</code> are the types of the arguments and <code>T</code>, is the return type of the private method.
     *
     * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
     * @tparam A1 the type of the argument 1
     * @tparam A2 the type of the argument 2
     * @tparam A3 the type of the argument 3
     * @tparam A4 the type of the argument 4
     * @tparam A5 the type of the argument 5
     * @tparam A6 the type of the argument 6
     * @tparam A7 the type of the argument 7
     * @tparam A8 the type of the argument 8
     * @tparam A9 the type of the argument 9
     * @tparam A10 the type of the argument 10
     * @tparam A11 the type of the argument 11
     * @tparam A12 the type of the argument 12
     * @tparam A13 the type of the argument 13
     * @tparam T the return type of the private method
     * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
     */
    def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, T](methodName: Symbol) = new PrivateMethod13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, T](methodName)
  }

  /**
   * Contains a factory method for instantiating <code>PrivateMethod14</code> objects.
   */
  object PrivateMethod14 {
    /**
     * Construct a new <code>PrivateMethod14</code> object with passed <code>methodName</code> symbol.
     * The type parameter, <code>A1 - A14</code> are the types of the arguments and <code>T</code>, is the return type of the private method.
     *
     * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
     * @tparam A1 the type of the argument 1
     * @tparam A2 the type of the argument 2
     * @tparam A3 the type of the argument 3
     * @tparam A4 the type of the argument 4
     * @tparam A5 the type of the argument 5
     * @tparam A6 the type of the argument 6
     * @tparam A7 the type of the argument 7
     * @tparam A8 the type of the argument 8
     * @tparam A9 the type of the argument 9
     * @tparam A10 the type of the argument 10
     * @tparam A11 the type of the argument 11
     * @tparam A12 the type of the argument 12
     * @tparam A13 the type of the argument 13
     * @tparam A14 the type of the argument 14
     * @tparam T the return type of the private method
     * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
     */
    def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, T](methodName: Symbol) = 
      new PrivateMethod14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, T](methodName)
  }

  /**
   * Contains a factory method for instantiating <code>PrivateMethod15</code> objects.
   */
  object PrivateMethod15 {
    /**
     * Construct a new <code>PrivateMethod15</code> object with passed <code>methodName</code> symbol.
     * The type parameter, <code>A1 - A15</code> are the types of the arguments and <code>T</code>, is the return type of the private method.
     *
     * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
     * @tparam A1 the type of the argument 1
     * @tparam A2 the type of the argument 2
     * @tparam A3 the type of the argument 3
     * @tparam A4 the type of the argument 4
     * @tparam A5 the type of the argument 5
     * @tparam A6 the type of the argument 6
     * @tparam A7 the type of the argument 7
     * @tparam A8 the type of the argument 8
     * @tparam A9 the type of the argument 9
     * @tparam A10 the type of the argument 10
     * @tparam A11 the type of the argument 11
     * @tparam A12 the type of the argument 12
     * @tparam A13 the type of the argument 13
     * @tparam A14 the type of the argument 14
     * @tparam A15 the type of the argument 15
     * @tparam T the return type of the private method
     * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
     */
    def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, T](methodName: Symbol) = 
      new PrivateMethod15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, T](methodName)
  }

  /**
   * Contains a factory method for instantiating <code>PrivateMethod16</code> objects.
   */
  object PrivateMethod16 {
    /**
     * Construct a new <code>PrivateMethod16</code> object with passed <code>methodName</code> symbol.
     * The type parameter, <code>A1 - A16</code> are the types of the arguments and <code>T</code>, is the return type of the private method.
     *
     * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
     * @tparam A1 the type of the argument 1
     * @tparam A2 the type of the argument 2
     * @tparam A3 the type of the argument 3
     * @tparam A4 the type of the argument 4
     * @tparam A5 the type of the argument 5
     * @tparam A6 the type of the argument 6
     * @tparam A7 the type of the argument 7
     * @tparam A8 the type of the argument 8
     * @tparam A9 the type of the argument 9
     * @tparam A10 the type of the argument 10
     * @tparam A11 the type of the argument 11
     * @tparam A12 the type of the argument 12
     * @tparam A13 the type of the argument 13
     * @tparam A14 the type of the argument 14
     * @tparam A15 the type of the argument 15
     * @tparam A16 the type of the argument 16
     * @tparam T the return type of the private method
     * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
     */
    def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, T](methodName: Symbol) = 
      new PrivateMethod16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, T](methodName)
  }

  /**
   * Contains a factory method for instantiating <code>PrivateMethod17</code> objects.
   */
  object PrivateMethod17 {
    /**
     * Construct a new <code>PrivateMethod17</code> object with passed <code>methodName</code> symbol.
     * The type parameter, <code>A1 - A17</code> are the types of the arguments and <code>T</code>, is the return type of the private method.
     *
     * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
     * @tparam A1 the type of the argument 1
     * @tparam A2 the type of the argument 2
     * @tparam A3 the type of the argument 3
     * @tparam A4 the type of the argument 4
     * @tparam A5 the type of the argument 5
     * @tparam A6 the type of the argument 6
     * @tparam A7 the type of the argument 7
     * @tparam A8 the type of the argument 8
     * @tparam A9 the type of the argument 9
     * @tparam A10 the type of the argument 10
     * @tparam A11 the type of the argument 11
     * @tparam A12 the type of the argument 12
     * @tparam A13 the type of the argument 13
     * @tparam A14 the type of the argument 14
     * @tparam A15 the type of the argument 15
     * @tparam A16 the type of the argument 16
     * @tparam A17 the type of the argument 17
     * @tparam T the return type of the private method
     * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
     */
    def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, T](methodName: Symbol) = 
      new PrivateMethod17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, T](methodName)
  }

  /**
   * Contains a factory method for instantiating <code>PrivateMethod18</code> objects.
   */
  object PrivateMethod18 {
    /**
     * Construct a new <code>PrivateMethod18</code> object with passed <code>methodName</code> symbol.
     * The type parameter, <code>A1 - A18</code> are the types of the arguments and <code>T</code>, is the return type of the private method.
     *
     * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
     * @tparam A1 the type of the argument 1
     * @tparam A2 the type of the argument 2
     * @tparam A3 the type of the argument 3
     * @tparam A4 the type of the argument 4
     * @tparam A5 the type of the argument 5
     * @tparam A6 the type of the argument 6
     * @tparam A7 the type of the argument 7
     * @tparam A8 the type of the argument 8
     * @tparam A9 the type of the argument 9
     * @tparam A10 the type of the argument 10
     * @tparam A11 the type of the argument 11
     * @tparam A12 the type of the argument 12
     * @tparam A13 the type of the argument 13
     * @tparam A14 the type of the argument 14
     * @tparam A15 the type of the argument 15
     * @tparam A16 the type of the argument 16
     * @tparam A17 the type of the argument 17
     * @tparam A18 the type of the argument 18
     * @tparam T the return type of the private method
     * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
     */
    def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, T](methodName: Symbol) = 
      new PrivateMethod18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, T](methodName)
  }

  /**
   * Contains a factory method for instantiating <code>PrivateMethod19</code> objects.
   */
  object PrivateMethod19 {
    /**
     * Construct a new <code>PrivateMethod19</code> object with passed <code>methodName</code> symbol.
     * The type parameter, <code>A1 - A19</code> are the types of the arguments and <code>T</code>, is the return type of the private method.
     *
     * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
     * @tparam A1 the type of the argument 1
     * @tparam A2 the type of the argument 2
     * @tparam A3 the type of the argument 3
     * @tparam A4 the type of the argument 4
     * @tparam A5 the type of the argument 5
     * @tparam A6 the type of the argument 6
     * @tparam A7 the type of the argument 7
     * @tparam A8 the type of the argument 8
     * @tparam A9 the type of the argument 9
     * @tparam A10 the type of the argument 10
     * @tparam A11 the type of the argument 11
     * @tparam A12 the type of the argument 12
     * @tparam A13 the type of the argument 13
     * @tparam A14 the type of the argument 14
     * @tparam A15 the type of the argument 15
     * @tparam A16 the type of the argument 16
     * @tparam A17 the type of the argument 17
     * @tparam A18 the type of the argument 18
     * @tparam A19 the type of the argument 19
     * @tparam T the return type of the private method
     * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
     */
    def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, T](methodName: Symbol) = 
      new PrivateMethod19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, T](methodName)
  }

  /**
   * Contains a factory method for instantiating <code>PrivateMethod20</code> objects.
   */
  object PrivateMethod20 {
    /**
     * Construct a new <code>PrivateMethod20</code> object with passed <code>methodName</code> symbol.
     * The type parameter, <code>A1 - A20</code> are the types of the arguments and <code>T</code>, is the return type of the private method.
     *
     * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
     * @tparam A1 the type of the argument 1
     * @tparam A2 the type of the argument 2
     * @tparam A3 the type of the argument 3
     * @tparam A4 the type of the argument 4
     * @tparam A5 the type of the argument 5
     * @tparam A6 the type of the argument 6
     * @tparam A7 the type of the argument 7
     * @tparam A8 the type of the argument 8
     * @tparam A9 the type of the argument 9
     * @tparam A10 the type of the argument 10
     * @tparam A11 the type of the argument 11
     * @tparam A12 the type of the argument 12
     * @tparam A13 the type of the argument 13
     * @tparam A14 the type of the argument 14
     * @tparam A15 the type of the argument 15
     * @tparam A16 the type of the argument 16
     * @tparam A17 the type of the argument 17
     * @tparam A18 the type of the argument 18
     * @tparam A19 the type of the argument 19
     * @tparam A20 the type of the argument 20
     * @tparam T the return type of the private method
     * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
     */
    def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, T](methodName: Symbol) = 
      new PrivateMethod20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, T](methodName)
  }

  /**
   * Contains a factory method for instantiating <code>PrivateMethod21</code> objects.
   */
  object PrivateMethod21 {
    /**
     * Construct a new <code>PrivateMethod21</code> object with passed <code>methodName</code> symbol.
     * The type parameter, <code>A1 - A21</code> are the types of the arguments and <code>T</code>, is the return type of the private method.
     *
     * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
     * @tparam A1 the type of the argument 1
     * @tparam A2 the type of the argument 2
     * @tparam A3 the type of the argument 3
     * @tparam A4 the type of the argument 4
     * @tparam A5 the type of the argument 5
     * @tparam A6 the type of the argument 6
     * @tparam A7 the type of the argument 7
     * @tparam A8 the type of the argument 8
     * @tparam A9 the type of the argument 9
     * @tparam A10 the type of the argument 10
     * @tparam A11 the type of the argument 11
     * @tparam A12 the type of the argument 12
     * @tparam A13 the type of the argument 13
     * @tparam A14 the type of the argument 14
     * @tparam A15 the type of the argument 15
     * @tparam A16 the type of the argument 16
     * @tparam A17 the type of the argument 17
     * @tparam A18 the type of the argument 18
     * @tparam A19 the type of the argument 19
     * @tparam A20 the type of the argument 20
     * @tparam A21 the type of the argument 21
     * @tparam T the return type of the private method
     * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
     */
    def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, T](methodName: Symbol) = 
      new PrivateMethod21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, T](methodName)
  }

  /**
   * Class whose instances represent an invocation of a private method. Instances of this
   * class contain the name of the private method (<code>methodName</code>) and the arguments
   * to pass to it during the invocation (<code>args</code>).
   * The type parameter, <code>T</code>, is the return type of the private method.
   *
   * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
   * @param args zero to many arguments to pass to the private method when invoked
   * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
   */
  final class Invocation[T](val methodName: Symbol, val args: Any*) {
    requireNonNull(methodName)
  }

  /**
  * A trait representing an invocation of a method with typed arguments.
  * Instances implementing this trait contain information about the method being invoked
  * and the type arguments and arguments value passed to it.
  */
  trait SafeInvocation {
    val methodName: Symbol
    val args: Array[Any]
  }

  /**
   * Class whose instances represent an invocation of a private method with 0 argument. Instances of this
   * class contain the name of the private method (<code>methodName</code>).
   * The type parameter, <code>T</code>, is the return type of the private method.
   *
   * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
   * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
   */
  final class Invocation0[T](val methodName: Symbol) extends SafeInvocation {
    requireNonNull(methodName)
    val args = Array.empty[Any]
  }

  /**
   * Class whose instances represent an invocation of a private method with 1 argument. Instances of this
   * class contain the name of the private method (<code>methodName</code>) and the 1 argument
   * to pass to it during the invocation (<code>arg1</code>).
   * The type parameter, <code>T</code>, is the return type of the private method.
   *
   * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
   * @param arg1 argument 1 to pass to the private method when invoked
   * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
   */
  final class Invocation1[A1, T](val methodName: Symbol, val arg1: A1) extends SafeInvocation {
    requireNonNull(methodName)
    val args = Array[Any](arg1)
  }

  /**
   * Class whose instances represent an invocation of a private method with 2 arguments. Instances of this
   * class contain the name of the private method (<code>methodName</code>) and the 2 arguments
   * to pass to it during the invocation (<code>arg1</code>, <code>arg2</code>).
   * The type parameter, <code>T</code>, is the return type of the private method.
   *
   * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
   * @param arg1 argument 1 to pass to the private method when invoked
   * @param arg2 argument 2 to pass to the private method when invoked
   * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
   */
  final class Invocation2[A1, A2, T](val methodName: Symbol, val arg1: A1, val arg2: A2) extends SafeInvocation {
    requireNonNull(methodName)
    val args = Array(arg1, arg2)
  }

  /**
   * Class whose instances represent an invocation of a private method with 3 arguments. Instances of this
   * class contain the name of the private method (<code>methodName</code>) and the 3 arguments
   * to pass to it during the invocation (<code>arg1</code>, <code>arg2</code>, <code>arg3</code>).
   * The type parameter, <code>T</code>, is the return type of the private method.
   *
   * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
   * @param arg1 argument 1 to pass to the private method when invoked
   * @param arg2 argument 2 to pass to the private method when invoked
   * @param arg3 argument 3 to pass to the private method when invoked
   * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
   */
  final class Invocation3[A1, A2, A3, T](val methodName: Symbol, val arg1: A1, val arg2: A2, val arg3: A3) extends SafeInvocation {
    requireNonNull(methodName)
    val args = Array(arg1, arg2, arg3)
  }

  /**
   * Class whose instances represent an invocation of a private method with 4 arguments. Instances of this
   * class contain the name of the private method (<code>methodName</code>) and the 4 arguments
   * to pass to it during the invocation (<code>arg1</code>, <code>arg2</code>, <code>arg3</code>, <code>arg4</code>).
   * The type parameter, <code>T</code>, is the return type of the private method.
   *
   * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
   * @param arg1 argument 1 to pass to the private method when invoked
   * @param arg2 argument 2 to pass to the private method when invoked
   * @param arg3 argument 3 to pass to the private method when invoked
   * @param arg4 argument 4 to pass to the private method when invoked
   * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
   */
  final class Invocation4[A1, A2, A3, A4, T](val methodName: Symbol, val arg1: A1, val arg2: A2, val arg3: A3, val arg4: A4) extends SafeInvocation {
    requireNonNull(methodName)
    val args = Array(arg1, arg2, arg3, arg4)
  }

  /**
   * Class whose instances represent an invocation of a private method with 5 arguments. Instances of this
   * class contain the name of the private method (<code>methodName</code>) and the 5 arguments
   * to pass to it during the invocation (<code>arg1</code>, <code>arg2</code>, <code>arg3</code>, <code>arg4</code>, 
   * <code>arg5</code>).
   * The type parameter, <code>T</code>, is the return type of the private method.
   *
   * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
   * @param arg1 argument 1 to pass to the private method when invoked
   * @param arg2 argument 2 to pass to the private method when invoked
   * @param arg3 argument 3 to pass to the private method when invoked
   * @param arg4 argument 4 to pass to the private method when invoked
   * @param arg5 argument 5 to pass to the private method when invoked
   * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
   */
  final class Invocation5[A1, A2, A3, A4, A5, T](val methodName: Symbol, val arg1: A1, val arg2: A2, val arg3: A3, val arg4: A4, val arg5: A5) extends SafeInvocation {
    requireNonNull(methodName)
    val args = Array(arg1, arg2, arg3, arg4, arg5)
  }

  /**
   * Class whose instances represent an invocation of a private method with 6 arguments. Instances of this
   * class contain the name of the private method (<code>methodName</code>) and the 6 arguments
   * to pass to it during the invocation (<code>arg1</code>, <code>arg2</code>, <code>arg3</code>, <code>arg4</code>, 
   * <code>arg5</code>, <code>arg6</code>).
   * The type parameter, <code>T</code>, is the return type of the private method.
   *
   * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
   * @param arg1 argument 1 to pass to the private method when invoked
   * @param arg2 argument 2 to pass to the private method when invoked
   * @param arg3 argument 3 to pass to the private method when invoked
   * @param arg4 argument 4 to pass to the private method when invoked
   * @param arg5 argument 5 to pass to the private method when invoked
   * @param arg6 argument 6 to pass to the private method when invoked
   * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
   */
  final class Invocation6[A1, A2, A3, A4, A5, A6, T](val methodName: Symbol, val arg1: A1, val arg2: A2, val arg3: A3, val arg4: A4, val arg5: A5, val arg6: A6) extends SafeInvocation {
    requireNonNull(methodName)
    val args = Array(arg1, arg2, arg3, arg4, arg5, arg6)
  }

  /**
   * Class whose instances represent an invocation of a private method with 7 arguments. Instances of this
   * class contain the name of the private method (<code>methodName</code>) and the 7 arguments
   * to pass to it during the invocation (<code>arg1</code>, <code>arg2</code>, <code>arg3</code>, <code>arg4</code>, 
   * <code>arg5</code>, <code>arg6</code>, <code>arg7</code>).
   * The type parameter, <code>T</code>, is the return type of the private method.
   *
   * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
   * @param arg1 argument 1 to pass to the private method when invoked
   * @param arg2 argument 2 to pass to the private method when invoked
   * @param arg3 argument 3 to pass to the private method when invoked
   * @param arg4 argument 4 to pass to the private method when invoked
   * @param arg5 argument 5 to pass to the private method when invoked
   * @param arg6 argument 6 to pass to the private method when invoked
   * @param arg7 argument 7 to pass to the private method when invoked
   * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
   */
  final class Invocation7[A1, A2, A3, A4, A5, A6, A7, T](val methodName: Symbol, val arg1: A1, val arg2: A2, val arg3: A3, val arg4: A4, val arg5: A5, val arg6: A6, 
                                                         val arg7: A7) extends SafeInvocation {
    requireNonNull(methodName)
    val args = Array(arg1, arg2, arg3, arg4, arg5, arg6, arg7)
  }

  /**
   * Class whose instances represent an invocation of a private method with 8 arguments. Instances of this
   * class contain the name of the private method (<code>methodName</code>) and the 8 arguments
   * to pass to it during the invocation (<code>arg1</code>, <code>arg2</code>, <code>arg3</code>, <code>arg4</code>, 
   * <code>arg5</code>, <code>arg6</code>, <code>arg7</code>, <code>arg8</code>).
   * The type parameter, <code>T</code>, is the return type of the private method.
   *
   * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
   * @param arg1 argument 1 to pass to the private method when invoked
   * @param arg2 argument 2 to pass to the private method when invoked
   * @param arg3 argument 3 to pass to the private method when invoked
   * @param arg4 argument 4 to pass to the private method when invoked
   * @param arg5 argument 5 to pass to the private method when invoked
   * @param arg6 argument 6 to pass to the private method when invoked
   * @param arg7 argument 7 to pass to the private method when invoked
   * @param arg8 argument 8 to pass to the private method when invoked
   * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
   */
  final class Invocation8[A1, A2, A3, A4, A5, A6, A7, A8, T](val methodName: Symbol, val arg1: A1, val arg2: A2, val arg3: A3, val arg4: A4, val arg5: A5, val arg6: A6, 
                                                         val arg7: A7, val arg8: A8) extends SafeInvocation {
    requireNonNull(methodName)
    val args = Array(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)
  }

  /**
   * Class whose instances represent an invocation of a private method with 9 arguments. Instances of this
   * class contain the name of the private method (<code>methodName</code>) and the 9 arguments
   * to pass to it during the invocation (<code>arg1</code>, <code>arg2</code>, <code>arg3</code>, <code>arg4</code>, 
   * <code>arg5</code>, <code>arg6</code>, <code>arg7</code>, <code>arg8</code>, <code>arg9</code>).
   * The type parameter, <code>T</code>, is the return type of the private method.
   *
   * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
   * @param arg1 argument 1 to pass to the private method when invoked
   * @param arg2 argument 2 to pass to the private method when invoked
   * @param arg3 argument 3 to pass to the private method when invoked
   * @param arg4 argument 4 to pass to the private method when invoked
   * @param arg5 argument 5 to pass to the private method when invoked
   * @param arg6 argument 6 to pass to the private method when invoked
   * @param arg7 argument 7 to pass to the private method when invoked
   * @param arg8 argument 8 to pass to the private method when invoked
   * @param arg9 argument 9 to pass to the private method when invoked
   * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
   */
  final class Invocation9[A1, A2, A3, A4, A5, A6, A7, A8, A9, T](val methodName: Symbol, val arg1: A1, val arg2: A2, val arg3: A3, val arg4: A4, val arg5: A5, val arg6: A6, 
                                                         val arg7: A7, val arg8: A8, arg9: A9) extends SafeInvocation {
    requireNonNull(methodName)
    val args = Array(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)
  }

  /**
   * Class whose instances represent an invocation of a private method with 10 arguments. Instances of this
   * class contain the name of the private method (<code>methodName</code>) and the 10 arguments
   * to pass to it during the invocation (<code>arg1</code>, <code>arg2</code>, <code>arg3</code>, <code>arg4</code>, 
   * <code>arg5</code>, <code>arg6</code>, <code>arg7</code>, <code>arg8</code>, <code>arg9</code>, <code>arg10</code>).
   * The type parameter, <code>T</code>, is the return type of the private method.
   *
   * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
   * @param arg1 argument 1 to pass to the private method when invoked
   * @param arg2 argument 2 to pass to the private method when invoked
   * @param arg3 argument 3 to pass to the private method when invoked
   * @param arg4 argument 4 to pass to the private method when invoked
   * @param arg5 argument 5 to pass to the private method when invoked
   * @param arg6 argument 6 to pass to the private method when invoked
   * @param arg7 argument 7 to pass to the private method when invoked
   * @param arg8 argument 8 to pass to the private method when invoked
   * @param arg9 argument 9 to pass to the private method when invoked
   * @param arg10 argument 10 to pass to the private method when invoked
   * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
   */
  final class Invocation10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, T](val methodName: Symbol, val arg1: A1, val arg2: A2, val arg3: A3, val arg4: A4, val arg5: A5, val arg6: A6, 
                                                         val arg7: A7, val arg8: A8, arg9: A9, arg10: A10) extends SafeInvocation {
    requireNonNull(methodName)
    val args = Array(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)
  }

  /**
   * Class whose instances represent an invocation of a private method with 11 arguments. Instances of this
   * class contain the name of the private method (<code>methodName</code>) and the 11 arguments
   * to pass to it during the invocation (<code>arg1</code>, <code>arg2</code>, <code>arg3</code>, <code>arg4</code>, 
   * <code>arg5</code>, <code>arg6</code>, <code>arg7</code>, <code>arg8</code>, <code>arg9</code>, <code>arg10</code>, 
   * <code>arg11</code>).
   * The type parameter, <code>T</code>, is the return type of the private method.
   *
   * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
   * @param arg1 argument 1 to pass to the private method when invoked
   * @param arg2 argument 2 to pass to the private method when invoked
   * @param arg3 argument 3 to pass to the private method when invoked
   * @param arg4 argument 4 to pass to the private method when invoked
   * @param arg5 argument 5 to pass to the private method when invoked
   * @param arg6 argument 6 to pass to the private method when invoked
   * @param arg7 argument 7 to pass to the private method when invoked
   * @param arg8 argument 8 to pass to the private method when invoked
   * @param arg9 argument 9 to pass to the private method when invoked
   * @param arg10 argument 10 to pass to the private method when invoked
   * @param arg11 argument 11 to pass to the private method when invoked
   * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
   */
  final class Invocation11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, T](val methodName: Symbol, val arg1: A1, val arg2: A2, val arg3: A3, val arg4: A4, val arg5: A5, val arg6: A6, 
                                                         val arg7: A7, val arg8: A8, val arg9: A9, val arg10: A10, val arg11: A11) extends SafeInvocation {
    requireNonNull(methodName)
    val args = Array(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11)
  }

  /**
   * Class whose instances represent an invocation of a private method with 12 arguments. Instances of this
   * class contain the name of the private method (<code>methodName</code>) and the 12 arguments
   * to pass to it during the invocation (<code>arg1</code>, <code>arg2</code>, <code>arg3</code>, <code>arg4</code>, 
   * <code>arg5</code>, <code>arg6</code>, <code>arg7</code>, <code>arg8</code>, <code>arg9</code>, <code>arg10</code>, 
   * <code>arg11</code>, <code>arg12</code>).
   * The type parameter, <code>T</code>, is the return type of the private method.
   *
   * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
   * @param arg1 argument 1 to pass to the private method when invoked
   * @param arg2 argument 2 to pass to the private method when invoked
   * @param arg3 argument 3 to pass to the private method when invoked
   * @param arg4 argument 4 to pass to the private method when invoked
   * @param arg5 argument 5 to pass to the private method when invoked
   * @param arg6 argument 6 to pass to the private method when invoked
   * @param arg7 argument 7 to pass to the private method when invoked
   * @param arg8 argument 8 to pass to the private method when invoked
   * @param arg9 argument 9 to pass to the private method when invoked
   * @param arg10 argument 10 to pass to the private method when invoked
   * @param arg11 argument 11 to pass to the private method when invoked
   * @param arg12 argument 12 to pass to the private method when invoked
   * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
   */
  final class Invocation12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, T](val methodName: Symbol, val arg1: A1, val arg2: A2, val arg3: A3, val arg4: A4, val arg5: A5, val arg6: A6, 
                                                         val arg7: A7, val arg8: A8, val arg9: A9, val arg10: A10, val arg11: A11, val arg12: A12) extends SafeInvocation {
    requireNonNull(methodName)
    val args = Array(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12)
  }

  /**
   * Class whose instances represent an invocation of a private method with 13 arguments. Instances of this
   * class contain the name of the private method (<code>methodName</code>) and the 13 arguments
   * to pass to it during the invocation (<code>arg1</code>, <code>arg2</code>, <code>arg3</code>, <code>arg4</code>, 
   * <code>arg5</code>, <code>arg6</code>, <code>arg7</code>, <code>arg8</code>, <code>arg9</code>, <code>arg10</code>, 
   * <code>arg11</code>, <code>arg12</code>, <code>arg13</code>).
   * The type parameter, <code>T</code>, is the return type of the private method.
   *
   * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
   * @param arg1 argument 1 to pass to the private method when invoked
   * @param arg2 argument 2 to pass to the private method when invoked
   * @param arg3 argument 3 to pass to the private method when invoked
   * @param arg4 argument 4 to pass to the private method when invoked
   * @param arg5 argument 5 to pass to the private method when invoked
   * @param arg6 argument 6 to pass to the private method when invoked
   * @param arg7 argument 7 to pass to the private method when invoked
   * @param arg8 argument 8 to pass to the private method when invoked
   * @param arg9 argument 9 to pass to the private method when invoked
   * @param arg10 argument 10 to pass to the private method when invoked
   * @param arg11 argument 11 to pass to the private method when invoked
   * @param arg12 argument 12 to pass to the private method when invoked
   * @param arg13 argument 13 to pass to the private method when invoked
   * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
   */
  final class Invocation13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, T](val methodName: Symbol, val arg1: A1, val arg2: A2, val arg3: A3, val arg4: A4, val arg5: A5, val arg6: A6, 
                                                         val arg7: A7, val arg8: A8, val arg9: A9, val arg10: A10, val arg11: A11, val arg12: A12, val arg13: A13) extends SafeInvocation {
    requireNonNull(methodName)
    val args = Array(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13)
  }

  /**
   * Class whose instances represent an invocation of a private method with 14 arguments. Instances of this
   * class contain the name of the private method (<code>methodName</code>) and the 14 arguments
   * to pass to it during the invocation (<code>arg1</code>, <code>arg2</code>, <code>arg3</code>, <code>arg4</code>, 
   * <code>arg5</code>, <code>arg6</code>, <code>arg7</code>, <code>arg8</code>, <code>arg9</code>, <code>arg10</code>, 
   * <code>arg11</code>, <code>arg12</code>, <code>arg13</code>, <code>arg14</code>).
   * The type parameter, <code>T</code>, is the return type of the private method.
   *
   * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
   * @param arg1 argument 1 to pass to the private method when invoked
   * @param arg2 argument 2 to pass to the private method when invoked
   * @param arg3 argument 3 to pass to the private method when invoked
   * @param arg4 argument 4 to pass to the private method when invoked
   * @param arg5 argument 5 to pass to the private method when invoked
   * @param arg6 argument 6 to pass to the private method when invoked
   * @param arg7 argument 7 to pass to the private method when invoked
   * @param arg8 argument 8 to pass to the private method when invoked
   * @param arg9 argument 9 to pass to the private method when invoked
   * @param arg10 argument 10 to pass to the private method when invoked
   * @param arg11 argument 11 to pass to the private method when invoked
   * @param arg12 argument 12 to pass to the private method when invoked
   * @param arg13 argument 13 to pass to the private method when invoked
   * @param arg14 argument 14 to pass to the private method when invoked
   * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
   */
  final class Invocation14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, T](val methodName: Symbol, val arg1: A1, val arg2: A2, val arg3: A3, val arg4: A4, val arg5: A5, val arg6: A6, 
                                                         val arg7: A7, val arg8: A8, val arg9: A9, val arg10: A10, val arg11: A11, val arg12: A12, val arg13: A13, val arg14: A14) extends SafeInvocation {
    requireNonNull(methodName)
    val args = Array(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14)
  }

  /**
   * Class whose instances represent an invocation of a private method with 15 arguments. Instances of this
   * class contain the name of the private method (<code>methodName</code>) and the 15 arguments
   * to pass to it during the invocation (<code>arg1</code>, <code>arg2</code>, <code>arg3</code>, <code>arg4</code>, 
   * <code>arg5</code>, <code>arg6</code>, <code>arg7</code>, <code>arg8</code>, <code>arg9</code>, <code>arg10</code>, 
   * <code>arg11</code>, <code>arg12</code>, <code>arg13</code>, <code>arg14</code>, <code>arg15</code>).
   * The type parameter, <code>T</code>, is the return type of the private method.
   *
   * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
   * @param arg1 argument 1 to pass to the private method when invoked
   * @param arg2 argument 2 to pass to the private method when invoked
   * @param arg3 argument 3 to pass to the private method when invoked
   * @param arg4 argument 4 to pass to the private method when invoked
   * @param arg5 argument 5 to pass to the private method when invoked
   * @param arg6 argument 6 to pass to the private method when invoked
   * @param arg7 argument 7 to pass to the private method when invoked
   * @param arg8 argument 8 to pass to the private method when invoked
   * @param arg9 argument 9 to pass to the private method when invoked
   * @param arg10 argument 10 to pass to the private method when invoked
   * @param arg11 argument 11 to pass to the private method when invoked
   * @param arg12 argument 12 to pass to the private method when invoked
   * @param arg13 argument 13 to pass to the private method when invoked
   * @param arg14 argument 14 to pass to the private method when invoked
   * @param arg15 argument 15 to pass to the private method when invoked
   * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
   */
  final class Invocation15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, T](val methodName: Symbol, val arg1: A1, val arg2: A2, val arg3: A3, val arg4: A4, val arg5: A5, val arg6: A6, 
                                                         val arg7: A7, val arg8: A8, val arg9: A9, val arg10: A10, val arg11: A11, val arg12: A12, val arg13: A13, val arg14: A14, val arg15: A15) extends SafeInvocation {
    requireNonNull(methodName)
    val args = Array(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15)
  }

  /**
   * Class whose instances represent an invocation of a private method with 16 arguments. Instances of this
   * class contain the name of the private method (<code>methodName</code>) and the 16 arguments
   * to pass to it during the invocation (<code>arg1</code>, <code>arg2</code>, <code>arg3</code>, <code>arg4</code>, 
   * <code>arg5</code>, <code>arg6</code>, <code>arg7</code>, <code>arg8</code>, <code>arg9</code>, <code>arg10</code>, 
   * <code>arg11</code>, <code>arg12</code>, <code>arg13</code>, <code>arg14</code>, <code>arg15</code>, <code>arg16</code>).
   * The type parameter, <code>T</code>, is the return type of the private method.
   *
   * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
   * @param arg1 argument 1 to pass to the private method when invoked
   * @param arg2 argument 2 to pass to the private method when invoked
   * @param arg3 argument 3 to pass to the private method when invoked
   * @param arg4 argument 4 to pass to the private method when invoked
   * @param arg5 argument 5 to pass to the private method when invoked
   * @param arg6 argument 6 to pass to the private method when invoked
   * @param arg7 argument 7 to pass to the private method when invoked
   * @param arg8 argument 8 to pass to the private method when invoked
   * @param arg9 argument 9 to pass to the private method when invoked
   * @param arg10 argument 10 to pass to the private method when invoked
   * @param arg11 argument 11 to pass to the private method when invoked
   * @param arg12 argument 12 to pass to the private method when invoked
   * @param arg13 argument 13 to pass to the private method when invoked
   * @param arg14 argument 14 to pass to the private method when invoked
   * @param arg15 argument 15 to pass to the private method when invoked
   * @param arg16 argument 16 to pass to the private method when invoked
   * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
   */
  final class Invocation16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, T](val methodName: Symbol, val arg1: A1, val arg2: A2, val arg3: A3, val arg4: A4, val arg5: A5, val arg6: A6, 
                                                         val arg7: A7, val arg8: A8, val arg9: A9, val arg10: A10, val arg11: A11, val arg12: A12, val arg13: A13, val arg14: A14, val arg15: A15, val arg16: A16) extends SafeInvocation {
    requireNonNull(methodName)
    val args = Array(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16)
  }

  /**
   * Class whose instances represent an invocation of a private method with 17 arguments. Instances of this
   * class contain the name of the private method (<code>methodName</code>) and the 17 arguments
   * to pass to it during the invocation (<code>arg1</code>, <code>arg2</code>, <code>arg3</code>, <code>arg4</code>, 
   * <code>arg5</code>, <code>arg6</code>, <code>arg7</code>, <code>arg8</code>, <code>arg9</code>, <code>arg10</code>, 
   * <code>arg11</code>, <code>arg12</code>, <code>arg13</code>, <code>arg14</code>, <code>arg15</code>, <code>arg16</code>, 
   * <code>arg17</code>).
   * The type parameter, <code>T</code>, is the return type of the private method.
   *
   * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
   * @param arg1 argument 1 to pass to the private method when invoked
   * @param arg2 argument 2 to pass to the private method when invoked
   * @param arg3 argument 3 to pass to the private method when invoked
   * @param arg4 argument 4 to pass to the private method when invoked
   * @param arg5 argument 5 to pass to the private method when invoked
   * @param arg6 argument 6 to pass to the private method when invoked
   * @param arg7 argument 7 to pass to the private method when invoked
   * @param arg8 argument 8 to pass to the private method when invoked
   * @param arg9 argument 9 to pass to the private method when invoked
   * @param arg10 argument 10 to pass to the private method when invoked
   * @param arg11 argument 11 to pass to the private method when invoked
   * @param arg12 argument 12 to pass to the private method when invoked
   * @param arg13 argument 13 to pass to the private method when invoked
   * @param arg14 argument 14 to pass to the private method when invoked
   * @param arg15 argument 15 to pass to the private method when invoked
   * @param arg16 argument 16 to pass to the private method when invoked
   * @param arg17 argument 17 to pass to the private method when invoked
   * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
   */
  final class Invocation17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, T](val methodName: Symbol, val arg1: A1, val arg2: A2, val arg3: A3, val arg4: A4, val arg5: A5, val arg6: A6, 
                                                         val arg7: A7, val arg8: A8, val arg9: A9, val arg10: A10, val arg11: A11, val arg12: A12, val arg13: A13, val arg14: A14, val arg15: A15, val arg16: A16, 
                                                         val arg17: A17) extends SafeInvocation {
    requireNonNull(methodName)
    val args = Array(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17)
  }

  /**
   * Class whose instances represent an invocation of a private method with 18 arguments. Instances of this
   * class contain the name of the private method (<code>methodName</code>) and the 18 arguments
   * to pass to it during the invocation (<code>arg1</code>, <code>arg2</code>, <code>arg3</code>, <code>arg4</code>, 
   * <code>arg5</code>, <code>arg6</code>, <code>arg7</code>, <code>arg8</code>, <code>arg9</code>, <code>arg10</code>, 
   * <code>arg11</code>, <code>arg12</code>, <code>arg13</code>, <code>arg14</code>, <code>arg15</code>, <code>arg16</code>, 
   * <code>arg17</code>, <code>arg18</code>).
   * The type parameter, <code>T</code>, is the return type of the private method.
   *
   * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
   * @param arg1 argument 1 to pass to the private method when invoked
   * @param arg2 argument 2 to pass to the private method when invoked
   * @param arg3 argument 3 to pass to the private method when invoked
   * @param arg4 argument 4 to pass to the private method when invoked
   * @param arg5 argument 5 to pass to the private method when invoked
   * @param arg6 argument 6 to pass to the private method when invoked
   * @param arg7 argument 7 to pass to the private method when invoked
   * @param arg8 argument 8 to pass to the private method when invoked
   * @param arg9 argument 9 to pass to the private method when invoked
   * @param arg10 argument 10 to pass to the private method when invoked
   * @param arg11 argument 11 to pass to the private method when invoked
   * @param arg12 argument 12 to pass to the private method when invoked
   * @param arg13 argument 13 to pass to the private method when invoked
   * @param arg14 argument 14 to pass to the private method when invoked
   * @param arg15 argument 15 to pass to the private method when invoked
   * @param arg16 argument 16 to pass to the private method when invoked
   * @param arg17 argument 17 to pass to the private method when invoked
   * @param arg18 argument 18 to pass to the private method when invoked
   * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
   */
  final class Invocation18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, T](val methodName: Symbol, val arg1: A1, val arg2: A2, val arg3: A3, val arg4: A4, val arg5: A5, val arg6: A6, 
                                                         val arg7: A7, val arg8: A8, val arg9: A9, val arg10: A10, val arg11: A11, val arg12: A12, val arg13: A13, val arg14: A14, val arg15: A15, val arg16: A16, 
                                                         val arg17: A17, val arg18: A18) extends SafeInvocation {
    requireNonNull(methodName)
    val args = Array(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18)
  }

  /**
   * Class whose instances represent an invocation of a private method with 19 arguments. Instances of this
   * class contain the name of the private method (<code>methodName</code>) and the 19 arguments
   * to pass to it during the invocation (<code>arg1</code>, <code>arg2</code>, <code>arg3</code>, <code>arg4</code>, 
   * <code>arg5</code>, <code>arg6</code>, <code>arg7</code>, <code>arg8</code>, <code>arg9</code>, <code>arg10</code>, 
   * <code>arg11</code>, <code>arg12</code>, <code>arg13</code>, <code>arg14</code>, <code>arg15</code>, <code>arg16</code>, 
   * <code>arg17</code>, <code>arg18</code>, <code>arg19</code>).
   * The type parameter, <code>T</code>, is the return type of the private method.
   *
   * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
   * @param arg1 argument 1 to pass to the private method when invoked
   * @param arg2 argument 2 to pass to the private method when invoked
   * @param arg3 argument 3 to pass to the private method when invoked
   * @param arg4 argument 4 to pass to the private method when invoked
   * @param arg5 argument 5 to pass to the private method when invoked
   * @param arg6 argument 6 to pass to the private method when invoked
   * @param arg7 argument 7 to pass to the private method when invoked
   * @param arg8 argument 8 to pass to the private method when invoked
   * @param arg9 argument 9 to pass to the private method when invoked
   * @param arg10 argument 10 to pass to the private method when invoked
   * @param arg11 argument 11 to pass to the private method when invoked
   * @param arg12 argument 12 to pass to the private method when invoked
   * @param arg13 argument 13 to pass to the private method when invoked
   * @param arg14 argument 14 to pass to the private method when invoked
   * @param arg15 argument 15 to pass to the private method when invoked
   * @param arg16 argument 16 to pass to the private method when invoked
   * @param arg17 argument 17 to pass to the private method when invoked
   * @param arg18 argument 18 to pass to the private method when invoked
   * @param arg19 argument 19 to pass to the private method when invoked
   * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
   */
  final class Invocation19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, T](val methodName: Symbol, val arg1: A1, val arg2: A2, val arg3: A3, val arg4: A4, val arg5: A5, val arg6: A6, 
                                                         val arg7: A7, val arg8: A8, val arg9: A9, val arg10: A10, val arg11: A11, val arg12: A12, val arg13: A13, val arg14: A14, val arg15: A15, val arg16: A16, 
                                                         val arg17: A17, val arg18: A18, val arg19: A19) extends SafeInvocation {
    requireNonNull(methodName)
    val args = Array(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19)
  }

  /**
   * Class whose instances represent an invocation of a private method with 20 arguments. Instances of this
   * class contain the name of the private method (<code>methodName</code>) and the 20 arguments
   * to pass to it during the invocation (<code>arg1</code>, <code>arg2</code>, <code>arg3</code>, <code>arg4</code>, 
   * <code>arg5</code>, <code>arg6</code>, <code>arg7</code>, <code>arg8</code>, <code>arg9</code>, <code>arg10</code>, 
   * <code>arg11</code>, <code>arg12</code>, <code>arg13</code>, <code>arg14</code>, <code>arg15</code>, <code>arg16</code>, 
   * <code>arg17</code>, <code>arg18</code>, <code>arg19</code>, <code>arg20</code>).
   * The type parameter, <code>T</code>, is the return type of the private method.
   *
   * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
   * @param arg1 argument 1 to pass to the private method when invoked
   * @param arg2 argument 2 to pass to the private method when invoked
   * @param arg3 argument 3 to pass to the private method when invoked
   * @param arg4 argument 4 to pass to the private method when invoked
   * @param arg5 argument 5 to pass to the private method when invoked
   * @param arg6 argument 6 to pass to the private method when invoked
   * @param arg7 argument 7 to pass to the private method when invoked
   * @param arg8 argument 8 to pass to the private method when invoked
   * @param arg9 argument 9 to pass to the private method when invoked
   * @param arg10 argument 10 to pass to the private method when invoked
   * @param arg11 argument 11 to pass to the private method when invoked
   * @param arg12 argument 12 to pass to the private method when invoked
   * @param arg13 argument 13 to pass to the private method when invoked
   * @param arg14 argument 14 to pass to the private method when invoked
   * @param arg15 argument 15 to pass to the private method when invoked
   * @param arg16 argument 16 to pass to the private method when invoked
   * @param arg17 argument 17 to pass to the private method when invoked
   * @param arg18 argument 18 to pass to the private method when invoked
   * @param arg19 argument 19 to pass to the private method when invoked
   * @param arg20 argument 20 to pass to the private method when invoked
   * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
   */
  final class Invocation20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, T](val methodName: Symbol, val arg1: A1, val arg2: A2, val arg3: A3, val arg4: A4, val arg5: A5, val arg6: A6, 
                                                         val arg7: A7, val arg8: A8, val arg9: A9, val arg10: A10, val arg11: A11, val arg12: A12, val arg13: A13, val arg14: A14, val arg15: A15, val arg16: A16, 
                                                         val arg17: A17, val arg18: A18, val arg19: A19, val arg20: A20) extends SafeInvocation {
    requireNonNull(methodName)
    val args = Array(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20)
  }

  /**
   * Class whose instances represent an invocation of a private method with 21 arguments. Instances of this
   * class contain the name of the private method (<code>methodName</code>) and the 21 arguments
   * to pass to it during the invocation (<code>arg1</code>, <code>arg2</code>, <code>arg3</code>, <code>arg4</code>, 
   * <code>arg5</code>, <code>arg6</code>, <code>arg7</code>, <code>arg8</code>, <code>arg9</code>, <code>arg10</code>, 
   * <code>arg11</code>, <code>arg12</code>, <code>arg13</code>, <code>arg14</code>, <code>arg15</code>, <code>arg16</code>, 
   * <code>arg17</code>, <code>arg18</code>, <code>arg19</code>, <code>arg20</code>, <code>arg21</code>).
   * The type parameter, <code>T</code>, is the return type of the private method.
   *
   * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
   * @param arg1 argument 1 to pass to the private method when invoked
   * @param arg2 argument 2 to pass to the private method when invoked
   * @param arg3 argument 3 to pass to the private method when invoked
   * @param arg4 argument 4 to pass to the private method when invoked
   * @param arg5 argument 5 to pass to the private method when invoked
   * @param arg6 argument 6 to pass to the private method when invoked
   * @param arg7 argument 7 to pass to the private method when invoked
   * @param arg8 argument 8 to pass to the private method when invoked
   * @param arg9 argument 9 to pass to the private method when invoked
   * @param arg10 argument 10 to pass to the private method when invoked
   * @param arg11 argument 11 to pass to the private method when invoked
   * @param arg12 argument 12 to pass to the private method when invoked
   * @param arg13 argument 13 to pass to the private method when invoked
   * @param arg14 argument 14 to pass to the private method when invoked
   * @param arg15 argument 15 to pass to the private method when invoked
   * @param arg16 argument 16 to pass to the private method when invoked
   * @param arg17 argument 17 to pass to the private method when invoked
   * @param arg18 argument 18 to pass to the private method when invoked
   * @param arg19 argument 19 to pass to the private method when invoked
   * @param arg20 argument 20 to pass to the private method when invoked
   * @param arg21 argument 21 to pass to the private method when invoked
   * @throws NullArgumentException if <code>methodName</code> is <code>null</code>
   */
  final class Invocation21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, T](val methodName: Symbol, val arg1: A1, val arg2: A2, val arg3: A3, val arg4: A4, val arg5: A5, val arg6: A6, 
                                                         val arg7: A7, val arg8: A8, val arg9: A9, val arg10: A10, val arg11: A11, val arg12: A12, val arg13: A13, val arg14: A14, val arg15: A15, val arg16: A16, 
                                                         val arg17: A17, val arg18: A18, val arg19: A19, val arg20: A20, val arg21: A21) extends SafeInvocation {
    requireNonNull(methodName)
    val args = Array(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21)
  }

  /**
   * Class used via an implicit conversion to enable private methods to be tested.
   */
  final class Invoker(target: AnyRef) {

    requireNonNull(target)

    /**
     * Invoke a private method. This method will attempt to invoke via reflection a private method.
     * The name of the method to invoke is contained in the <code>methodName</code> field of the passed <code>Invocation</code>.
     * The arguments to pass are contained in the <code>args</code> field. The object on which to invoke the private
     * method is the <code>target</code> object passed to this <code>Invoker</code>'s primary constructor.
     * The type parameter, <code>T</code>, is the return type of the private method.
     *
     * @param invocation the <code>Invocation</code> object containing the method name symbol and args of the invocation.
     * @return the value returned by the invoked private method
     * @throws IllegalArgumentException if the target object does not have a method of the name, with argument types
     * compatible with the objects in the passed args array, specified in the passed <code>Invocation</code> object.
     */
    def invokePrivate[T](invocation: Invocation[T]): T = {
      import invocation._

      // If 'getMessage passed as methodName, methodNameToInvoke would be "getMessage"
      val methodNameToInvoke = methodName.name

      def isMethodToInvoke(m: Method) = {

        val isInstanceMethod = !Modifier.isStatic(m.getModifiers())
        val simpleName = m.getName
        val paramTypes = m.getParameterTypes
        val isPrivate = Modifier.isPrivate(m.getModifiers())

        // The AnyVals must go in as Java wrapper types. But the type is still Any, so this needs to be converted
        // to AnyRef for the compiler to be happy. Implicit conversions are ambiguous, and really all that's needed
        // is a type cast, so I use isInstanceOf.
        def argsHaveValidTypes: Boolean = {

          // First, the arrays must have the same length:
          if (args.length == paramTypes.length) {
            val zipped = args.toList zip paramTypes.toList
  
            // If arg.asInstanceOf[AnyRef] has class java.lang.Integer, this needs to match the paramType Class instance for int

            def argMatchesParamType(arg: Any, paramType: Class[_]) = {
              // note that arg might be null, which is assignable to any reference type
              Option(arg.asInstanceOf[AnyRef]).fold (true) { anyRefArg =>
                paramType match {
                  case java.lang.Long.TYPE => anyRefArg.getClass == classOf[java.lang.Long]
                  case java.lang.Integer.TYPE => anyRefArg.getClass == classOf[java.lang.Integer]
                  case java.lang.Short.TYPE => anyRefArg.getClass == classOf[java.lang.Short]
                  case java.lang.Byte.TYPE => anyRefArg.getClass == classOf[java.lang.Byte]
                  case java.lang.Character.TYPE => anyRefArg.getClass == classOf[java.lang.Character]
                  case java.lang.Double.TYPE => anyRefArg.getClass == classOf[java.lang.Double]
                  case java.lang.Float.TYPE => anyRefArg.getClass == classOf[java.lang.Float]
                  case java.lang.Boolean.TYPE => anyRefArg.getClass == classOf[java.lang.Boolean]
                  case _ => paramType.isAssignableFrom(anyRefArg.getClass)
                }
              }
            }
            
            // The args classes need only be assignable to the parameter type. So therefore the parameter type
            // must be assignable *from* the corresponding arg class type.
            val invalidArgs =
              for ((arg, paramType) <- zipped if !argMatchesParamType(arg, paramType)) yield arg
            invalidArgs.length == 0
          }
          else false
        }

        /*
        The rules may be that private mehods in standalone objects currently get name mangled and made public,
        perhaps because there are two versions of each private method, one in the actual singleton and one int
        the class that also has static methods, and one calls the other. So if this is true, then I may change this
        to say if simpleName matches exactly and its private, or if ends with simpleName prepended by two dollar signs,
        then let it be public, but look for whatever the Scala compiler puts in there to mark it as private at the Scala source level.

        // org$scalatest$FailureMessages$$decorateToStringValue
        // 0 org$scalatest$FailureMessages$$decorateToStringValue
        [java] 1 true
        [java] 2 false
        [java] false
        [java] false
        [java] ^&^&^&^&^&^& invalidArgs.length is: 0
        [java] 5 true

        println("0 "+ simpleName)
        println("1 "+ isInstanceMethod)
        println("2 "+ isPrivate)
        println("3 "+ simpleName == methodNameToInvoke)
        println("4 "+ candidateResultType == resultType)
        println("5 "+ argsHaveValidTypes)

        This ugliness. I'll ignore the result type for now. Sheesh. Investigate that one. And I'll
        have to ignore private too for now, because in the bytecodes it isn't even private. And I'll
        also allow methods that end with $$<simpleName> if the simpleName doesn't match
        */

        isInstanceMethod && (simpleName == methodNameToInvoke || simpleName.endsWith("$$"+ methodNameToInvoke)) && argsHaveValidTypes
      }
  
      // Store in an array, because may have both isEmpty and empty, in which case I
      // will throw an exception.
      val methodArray =
        for (m <- target.getClass.getDeclaredMethods; if isMethodToInvoke(m))
          yield m
  
      if (methodArray.length == 0)
        throw new IllegalArgumentException("Can't find a private method named: " + methodNameToInvoke)
      else if (methodArray.length > 1)
        throw new IllegalArgumentException("Found two methods")
      else {
        val anyRefArgs = // Need to box these myself, because that's invoke is expecting an Array[Object], which maps to an Array[AnyRef]
          for (arg <- args) yield arg match {
            case anyRef: AnyRef => anyRef
            case null => null
            case any => any.asInstanceOf[AnyRef]
          }
        val privateMethodToInvoke = methodArray(0)
        privateMethodToInvoke.setAccessible(true)
        try {
          privateMethodToInvoke.invoke(target, anyRefArgs.toArray: _*).asInstanceOf[T]
        }
        catch {
          case e: InvocationTargetException =>
            val cause = e.getCause
            if (cause != null) throw cause else throw e
        }
      }
    }

    /**
     * Invoke a private method with no argument. This method will attempt to invoke via reflection a private method.
     * The name of the method to invoke is contained in the <code>methodName</code> field of the passed <code>Invocation</code>.
     * The object on which to invoke the private method is the <code>target</code> object passed to this <code>Invoker</code>'s 
     * primary constructor.  The type parameter, <code>T</code>, is the return type of the private method.
     *
     * @param invocation the <code>Invocation0</code> object containing the method name symbol and args of the invocation.
     * @return the value returned by the invoked private method
     * @throws IllegalArgumentException if the target object does not have a method of the name, with argument types
     * compatible with the objects in the passed args array, specified in the passed <code>Invocation</code> object.
     */
    def invokePrivate[T](invocation: Invocation0[T]): T = invokePrivateImpl(invocation)
    
    /**
     * Invoke a private method with 1 argument. This method will attempt to invoke via reflection a private method.
     * The name of the method to invoke is contained in the <code>methodName</code> field of the passed <code>Invocation</code>.
     * The arguments to pass are contained in the <code>args</code> field. The object on which to invoke the private
     * method is the <code>target</code> object passed to this <code>Invoker</code>'s primary constructor.
     * The type parameter, <code>T</code>, is the return type of the private method.
     *
     * @param invocation the <code>Invocation1</code> object containing the method name symbol and args of the invocation.
     * @return the value returned by the invoked private method
     * @throws IllegalArgumentException if the target object does not have a method of the name, with argument types
     * compatible with the objects in the passed args array, specified in the passed <code>Invocation</code> object.
     */
    def invokePrivate[A1, T](invocation: Invocation1[A1, T]): T = invokePrivateImpl(invocation)
    
    /**
     * Invoke a private method with 2 arguments. This method will attempt to invoke via reflection a private method.
     * The name of the method to invoke is contained in the <code>methodName</code> field of the passed <code>Invocation</code>.
     * The arguments to pass are contained in the <code>args</code> field. The object on which to invoke the private
     * method is the <code>target</code> object passed to this <code>Invoker</code>'s primary constructor.
     * The type parameter, <code>T</code>, is the return type of the private method.
     *
     * @param invocation the <code>Invocation2</code> object containing the method name symbol and args of the invocation.
     * @return the value returned by the invoked private method
     * @throws IllegalArgumentException if the target object does not have a method of the name, with argument types
     * compatible with the objects in the passed args array, specified in the passed <code>Invocation</code> object.
     */
    def invokePrivate[A1, A2, T](invocation: Invocation2[A1, A2, T]): T = invokePrivateImpl(invocation)
    
    /**
     * Invoke a private method with 3 arguments. This method will attempt to invoke via reflection a private method.
     * The name of the method to invoke is contained in the <code>methodName</code> field of the passed <code>Invocation</code>.
     * The arguments to pass are contained in the <code>args</code> field. The object on which to invoke the private
     * method is the <code>target</code> object passed to this <code>Invoker</code>'s primary constructor.
     * The type parameter, <code>T</code>, is the return type of the private method.
     *
     * @param invocation the <code>Invocation3</code> object containing the method name symbol and args of the invocation.
     * @return the value returned by the invoked private method
     * @throws IllegalArgumentException if the target object does not have a method of the name, with argument types
     * compatible with the objects in the passed args array, specified in the passed <code>Invocation</code> object.
     */
    def invokePrivate[A1, A2, A3, T](invocation: Invocation3[A1, A2, A3, T]): T = invokePrivateImpl(invocation)
    
    /**
     * Invoke a private method with 4 arguments. This method will attempt to invoke via reflection a private method.
     * The name of the method to invoke is contained in the <code>methodName</code> field of the passed <code>Invocation</code>.
     * The arguments to pass are contained in the <code>args</code> field. The object on which to invoke the private
     * method is the <code>target</code> object passed to this <code>Invoker</code>'s primary constructor.
     * The type parameter, <code>T</code>, is the return type of the private method.
     *
     * @param invocation the <code>Invocation4</code> object containing the method name symbol and args of the invocation.
     * @return the value returned by the invoked private method
     * @throws IllegalArgumentException if the target object does not have a method of the name, with argument types
     * compatible with the objects in the passed args array, specified in the passed <code>Invocation</code> object.
     */
    def invokePrivate[A1, A2, A3, A4, T](invocation: Invocation4[A1, A2, A3, A4, T]): T = invokePrivateImpl(invocation)
    
    /**
     * Invoke a private method with 5 arguments. This method will attempt to invoke via reflection a private method.
     * The name of the method to invoke is contained in the <code>methodName</code> field of the passed <code>Invocation</code>.
     * The arguments to pass are contained in the <code>args</code> field. The object on which to invoke the private
     * method is the <code>target</code> object passed to this <code>Invoker</code>'s primary constructor.
     * The type parameter, <code>T</code>, is the return type of the private method.
     *
     * @param invocation the <code>Invocation5</code> object containing the method name symbol and args of the invocation.
     * @return the value returned by the invoked private method
     * @throws IllegalArgumentException if the target object does not have a method of the name, with argument types
     * compatible with the objects in the passed args array, specified in the passed <code>Invocation</code> object.
     */
    def invokePrivate[A1, A2, A3, A4, A5, T](invocation: Invocation5[A1, A2, A3, A4, A5, T]): T = invokePrivateImpl(invocation)
    
    /**
     * Invoke a private method with 6 arguments. This method will attempt to invoke via reflection a private method.
     * The name of the method to invoke is contained in the <code>methodName</code> field of the passed <code>Invocation</code>.
     * The arguments to pass are contained in the <code>args</code> field. The object on which to invoke the private
     * method is the <code>target</code> object passed to this <code>Invoker</code>'s primary constructor.
     * The type parameter, <code>T</code>, is the return type of the private method.
     *
     * @param invocation the <code>Invocation6</code> object containing the method name symbol and args of the invocation.
     * @return the value returned by the invoked private method
     * @throws IllegalArgumentException if the target object does not have a method of the name, with argument types
     * compatible with the objects in the passed args array, specified in the passed <code>Invocation</code> object.
     */
    def invokePrivate[A1, A2, A3, A4, A5, A6, T](invocation: Invocation6[A1, A2, A3, A4, A5, A6, T]): T = invokePrivateImpl(invocation)

    /**
     * Invoke a private method with 7 arguments. This method will attempt to invoke via reflection a private method.
     * The name of the method to invoke is contained in the <code>methodName</code> field of the passed <code>Invocation</code>.
     * The arguments to pass are contained in the <code>args</code> field. The object on which to invoke the private
     * method is the <code>target</code> object passed to this <code>Invoker</code>'s primary constructor.
     * The type parameter, <code>T</code>, is the return type of the private method.
     *
     * @param invocation the <code>Invocation7</code> object containing the method name symbol and args of the invocation.
     * @return the value returned by the invoked private method
     * @throws IllegalArgumentException if the target object does not have a method of the name, with argument types
     * compatible with the objects in the passed args array, specified in the passed <code>Invocation</code> object.
     */
    def invokePrivate[A1, A2, A3, A4, A5, A6, A7, T](invocation: Invocation7[A1, A2, A3, A4, A5, A6, A7, T]): T = invokePrivateImpl(invocation)

    /**
     * Invoke a private method with 8 arguments. This method will attempt to invoke via reflection a private method.
     * The name of the method to invoke is contained in the <code>methodName</code> field of the passed <code>Invocation</code>.
     * The arguments to pass are contained in the <code>args</code> field. The object on which to invoke the private
     * method is the <code>target</code> object passed to this <code>Invoker</code>'s primary constructor.
     * The type parameter, <code>T</code>, is the return type of the private method.
     *
     * @param invocation the <code>Invocation8</code> object containing the method name symbol and args of the invocation.
     * @return the value returned by the invoked private method
     * @throws IllegalArgumentException if the target object does not have a method of the name, with argument types
     * compatible with the objects in the passed args array, specified in the passed <code>Invocation</code> object.
     */
    def invokePrivate[A1, A2, A3, A4, A5, A6, A7, A8, T](invocation: Invocation8[A1, A2, A3, A4, A5, A6, A7, A8, T]): T = invokePrivateImpl(invocation)

    /**
     * Invoke a private method with 9 arguments. This method will attempt to invoke via reflection a private method.
     * The name of the method to invoke is contained in the <code>methodName</code> field of the passed <code>Invocation</code>.
     * The arguments to pass are contained in the <code>args</code> field. The object on which to invoke the private
     * method is the <code>target</code> object passed to this <code>Invoker</code>'s primary constructor.
     * The type parameter, <code>T</code>, is the return type of the private method.
     *
     * @param invocation the <code>Invocation9</code> object containing the method name symbol and args of the invocation.
     * @return the value returned by the invoked private method
     * @throws IllegalArgumentException if the target object does not have a method of the name, with argument types
     * compatible with the objects in the passed args array, specified in the passed <code>Invocation</code> object.
     */
    def invokePrivate[A1, A2, A3, A4, A5, A6, A7, A8, A9, T](invocation: Invocation9[A1, A2, A3, A4, A5, A6, A7, A8, A9, T]): T = invokePrivateImpl(invocation)

    /**
     * Invoke a private method with 10 arguments. This method will attempt to invoke via reflection a private method.
     * The name of the method to invoke is contained in the <code>methodName</code> field of the passed <code>Invocation</code>.
     * The arguments to pass are contained in the <code>args</code> field. The object on which to invoke the private
     * method is the <code>target</code> object passed to this <code>Invoker</code>'s primary constructor.
     * The type parameter, <code>T</code>, is the return type of the private method.
     *
     * @param invocation the <code>Invocation10</code> object containing the method name symbol and args of the invocation.
     * @return the value returned by the invoked private method
     * @throws IllegalArgumentException if the target object does not have a method of the name, with argument types
     * compatible with the objects in the passed args array, specified in the passed <code>Invocation</code> object.
     */
    def invokePrivate[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, T](invocation: Invocation10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, T]): T = invokePrivateImpl(invocation)

    /**
     * Invoke a private method with 11 arguments. This method will attempt to invoke via reflection a private method.
     * The name of the method to invoke is contained in the <code>methodName</code> field of the passed <code>Invocation</code>.
     * The arguments to pass are contained in the <code>args</code> field. The object on which to invoke the private
     * method is the <code>target</code> object passed to this <code>Invoker</code>'s primary constructor.
     * The type parameter, <code>T</code>, is the return type of the private method.
     *
     * @param invocation the <code>Invocation11</code> object containing the method name symbol and args of the invocation.
     * @return the value returned by the invoked private method
     * @throws IllegalArgumentException if the target object does not have a method of the name, with argument types
     * compatible with the objects in the passed args array, specified in the passed <code>Invocation</code> object.
     */
    def invokePrivate[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, T](invocation: Invocation11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, T]): T = invokePrivateImpl(invocation)

    /**
     * Invoke a private method with 12 arguments. This method will attempt to invoke via reflection a private method.
     * The name of the method to invoke is contained in the <code>methodName</code> field of the passed <code>Invocation</code>.
     * The arguments to pass are contained in the <code>args</code> field. The object on which to invoke the private
     * method is the <code>target</code> object passed to this <code>Invoker</code>'s primary constructor.
     * The type parameter, <code>T</code>, is the return type of the private method.
     *
     * @param invocation the <code>Invocation12</code> object containing the method name symbol and args of the invocation.
     * @return the value returned by the invoked private method
     * @throws IllegalArgumentException if the target object does not have a method of the name, with argument types
     * compatible with the objects in the passed args array, specified in the passed <code>Invocation</code> object.
     */
    def invokePrivate[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, T](invocation: Invocation12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, T]): T = invokePrivateImpl(invocation)

    /**
     * Invoke a private method with 13 arguments. This method will attempt to invoke via reflection a private method.
     * The name of the method to invoke is contained in the <code>methodName</code> field of the passed <code>Invocation</code>.
     * The arguments to pass are contained in the <code>args</code> field. The object on which to invoke the private
     * method is the <code>target</code> object passed to this <code>Invoker</code>'s primary constructor.
     * The type parameter, <code>T</code>, is the return type of the private method.
     *
     * @param invocation the <code>Invocation13</code> object containing the method name symbol and args of the invocation.
     * @return the value returned by the invoked private method
     * @throws IllegalArgumentException if the target object does not have a method of the name, with argument types
     * compatible with the objects in the passed args array, specified in the passed <code>Invocation</code> object.
     */
    def invokePrivate[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, T](invocation: Invocation13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, T]): T = invokePrivateImpl(invocation)

    /**
     * Invoke a private method with 14 arguments. This method will attempt to invoke via reflection a private method.
     * The name of the method to invoke is contained in the <code>methodName</code> field of the passed <code>Invocation</code>.
     * The arguments to pass are contained in the <code>args</code> field. The object on which to invoke the private
     * method is the <code>target</code> object passed to this <code>Invoker</code>'s primary constructor.
     * The type parameter, <code>T</code>, is the return type of the private method.
     *
     * @param invocation the <code>Invocation14</code> object containing the method name symbol and args of the invocation.
     * @return the value returned by the invoked private method
     * @throws IllegalArgumentException if the target object does not have a method of the name, with argument types
     * compatible with the objects in the passed args array, specified in the passed <code>Invocation</code> object.
     */
    def invokePrivate[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, T](invocation: Invocation14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, T]): T = invokePrivateImpl(invocation)

    /**
     * Invoke a private method with 15 arguments. This method will attempt to invoke via reflection a private method.
     * The name of the method to invoke is contained in the <code>methodName</code> field of the passed <code>Invocation</code>.
     * The arguments to pass are contained in the <code>args</code> field. The object on which to invoke the private
     * method is the <code>target</code> object passed to this <code>Invoker</code>'s primary constructor.
     * The type parameter, <code>T</code>, is the return type of the private method.
     *
     * @param invocation the <code>Invocation15</code> object containing the method name symbol and args of the invocation.
     * @return the value returned by the invoked private method
     * @throws IllegalArgumentException if the target object does not have a method of the name, with argument types
     * compatible with the objects in the passed args array, specified in the passed <code>Invocation</code> object.
     */
    def invokePrivate[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, T](invocation: Invocation15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, T]): T = invokePrivateImpl(invocation)

    /**
     * Invoke a private method with 16 arguments. This method will attempt to invoke via reflection a private method.
     * The name of the method to invoke is contained in the <code>methodName</code> field of the passed <code>Invocation</code>.
     * The arguments to pass are contained in the <code>args</code> field. The object on which to invoke the private
     * method is the <code>target</code> object passed to this <code>Invoker</code>'s primary constructor.
     * The type parameter, <code>T</code>, is the return type of the private method.
     *
     * @param invocation the <code>Invocation16</code> object containing the method name symbol and args of the invocation.
     * @return the value returned by the invoked private method
     * @throws IllegalArgumentException if the target object does not have a method of the name, with argument types
     * compatible with the objects in the passed args array, specified in the passed <code>Invocation</code> object.
     */
    def invokePrivate[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, T](invocation: Invocation16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, T]): T = invokePrivateImpl(invocation)

    /**
     * Invoke a private method with 17 arguments. This method will attempt to invoke via reflection a private method.
     * The name of the method to invoke is contained in the <code>methodName</code> field of the passed <code>Invocation</code>.
     * The arguments to pass are contained in the <code>args</code> field. The object on which to invoke the private
     * method is the <code>target</code> object passed to this <code>Invoker</code>'s primary constructor.
     * The type parameter, <code>T</code>, is the return type of the private method.
     *
     * @param invocation the <code>Invocation17</code> object containing the method name symbol and args of the invocation.
     * @return the value returned by the invoked private method
     * @throws IllegalArgumentException if the target object does not have a method of the name, with argument types
     * compatible with the objects in the passed args array, specified in the passed <code>Invocation</code> object.
     */
    def invokePrivate[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, T](invocation: Invocation17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, T]): T = invokePrivateImpl(invocation)

    /**
     * Invoke a private method with 18 arguments. This method will attempt to invoke via reflection a private method.
     * The name of the method to invoke is contained in the <code>methodName</code> field of the passed <code>Invocation</code>.
     * The arguments to pass are contained in the <code>args</code> field. The object on which to invoke the private
     * method is the <code>target</code> object passed to this <code>Invoker</code>'s primary constructor.
     * The type parameter, <code>T</code>, is the return type of the private method.
     *
     * @param invocation the <code>Invocation18</code> object containing the method name symbol and args of the invocation.
     * @return the value returned by the invoked private method
     * @throws IllegalArgumentException if the target object does not have a method of the name, with argument types
     * compatible with the objects in the passed args array, specified in the passed <code>Invocation</code> object.
     */
    def invokePrivate[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, T](invocation: Invocation18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, T]): T = invokePrivateImpl(invocation)

    /**
     * Invoke a private method with 19 arguments. This method will attempt to invoke via reflection a private method.
     * The name of the method to invoke is contained in the <code>methodName</code> field of the passed <code>Invocation</code>.
     * The arguments to pass are contained in the <code>args</code> field. The object on which to invoke the private
     * method is the <code>target</code> object passed to this <code>Invoker</code>'s primary constructor.
     * The type parameter, <code>T</code>, is the return type of the private method.
     *
     * @param invocation the <code>Invocation19</code> object containing the method name symbol and args of the invocation.
     * @return the value returned by the invoked private method
     * @throws IllegalArgumentException if the target object does not have a method of the name, with argument types
     * compatible with the objects in the passed args array, specified in the passed <code>Invocation</code> object.
     */
    def invokePrivate[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, T](invocation: Invocation19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, T]): T = invokePrivateImpl(invocation)

    /**
     * Invoke a private method with 20 arguments. This method will attempt to invoke via reflection a private method.
     * The name of the method to invoke is contained in the <code>methodName</code> field of the passed <code>Invocation</code>.
     * The arguments to pass are contained in the <code>args</code> field. The object on which to invoke the private
     * method is the <code>target</code> object passed to this <code>Invoker</code>'s primary constructor.
     * The type parameter, <code>T</code>, is the return type of the private method.
     *
     * @param invocation the <code>Invocation20</code> object containing the method name symbol and args of the invocation.
     * @return the value returned by the invoked private method
     * @throws IllegalArgumentException if the target object does not have a method of the name, with argument types
     * compatible with the objects in the passed args array, specified in the passed <code>Invocation</code> object.
     */
    def invokePrivate[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, T](invocation: Invocation20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, T]): T = invokePrivateImpl(invocation)

    /**
     * Invoke a private method with 21 arguments. This method will attempt to invoke via reflection a private method.
     * The name of the method to invoke is contained in the <code>methodName</code> field of the passed <code>Invocation</code>.
     * The arguments to pass are contained in the <code>args</code> field. The object on which to invoke the private
     * method is the <code>target</code> object passed to this <code>Invoker</code>'s primary constructor.
     * The type parameter, <code>T</code>, is the return type of the private method.
     *
     * @param invocation the <code>Invocation21</code> object containing the method name symbol and args of the invocation.
     * @return the value returned by the invoked private method
     * @throws IllegalArgumentException if the target object does not have a method of the name, with argument types
     * compatible with the objects in the passed args array, specified in the passed <code>Invocation</code> object.
     */
    def invokePrivate[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, T](invocation: Invocation21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, T]): T = invokePrivateImpl(invocation)
    
    /**
     * Invoke a private method. This method will attempt to invoke via reflection a private method.
     * The name of the method to invoke is contained in the <code>methodName</code> field of the passed <code>Invocation</code>.
     * The arguments to pass are contained in the <code>args</code> field. The object on which to invoke the private
     * method is the <code>target</code> object passed to this <code>Invoker</code>'s primary constructor.
     * The type parameter, <code>T</code>, is the return type of the private method.
     *
     * @param invocation the <code>Invocation</code> object containing the method name symbol and args of the invocation.
     * @return the value returned by the invoked private method
     * @throws IllegalArgumentException if the target object does not have a method of the name, with argument types
     * compatible with the objects in the passed args array, specified in the passed <code>Invocation</code> object.
     */
    private def invokePrivateImpl[T](invocation: SafeInvocation): T = {
      import invocation._

      // If 'getMessage passed as methodName, methodNameToInvoke would be "getMessage"
      val methodNameToInvoke = methodName.name

      def isMethodToInvoke(m: Method) = {

        val isInstanceMethod = !Modifier.isStatic(m.getModifiers())
        val simpleName = m.getName
        val paramTypes = m.getParameterTypes
        val isPrivate = Modifier.isPrivate(m.getModifiers())

        // The AnyVals must go in as Java wrapper types. But the type is still Any, so this needs to be converted
        // to AnyRef for the compiler to be happy. Implicit conversions are ambiguous, and really all that's needed
        // is a type cast, so I use isInstanceOf.
        def argsHaveValidTypes: Boolean = {

          // First, the arrays must have the same length:
          if (args.length == paramTypes.length) {
            val zipped = args.toList zip paramTypes.toList
  
            // If arg.asInstanceOf[AnyRef] has class java.lang.Integer, this needs to match the paramType Class instance for int

            def argMatchesParamType(arg: Any, paramType: Class[_]) = {
              // note that arg might be null, which is assignable to any reference type
              Option(arg.asInstanceOf[AnyRef]).fold (true) { anyRefArg =>
                paramType match {
                  case java.lang.Long.TYPE => anyRefArg.getClass == classOf[java.lang.Long]
                  case java.lang.Integer.TYPE => anyRefArg.getClass == classOf[java.lang.Integer]
                  case java.lang.Short.TYPE => anyRefArg.getClass == classOf[java.lang.Short]
                  case java.lang.Byte.TYPE => anyRefArg.getClass == classOf[java.lang.Byte]
                  case java.lang.Character.TYPE => anyRefArg.getClass == classOf[java.lang.Character]
                  case java.lang.Double.TYPE => anyRefArg.getClass == classOf[java.lang.Double]
                  case java.lang.Float.TYPE => anyRefArg.getClass == classOf[java.lang.Float]
                  case java.lang.Boolean.TYPE => anyRefArg.getClass == classOf[java.lang.Boolean]
                  case _ => paramType.isAssignableFrom(anyRefArg.getClass)
                }
              }
            }
            
            // The args classes need only be assignable to the parameter type. So therefore the parameter type
            // must be assignable *from* the corresponding arg class type.
            val invalidArgs =
              for ((arg, paramType) <- zipped if !argMatchesParamType(arg, paramType)) yield arg
            invalidArgs.length == 0
          }
          else false
        }

        /*
        The rules may be that private mehods in standalone objects currently get name mangled and made public,
        perhaps because there are two versions of each private method, one in the actual singleton and one int
        the class that also has static methods, and one calls the other. So if this is true, then I may change this
        to say if simpleName matches exactly and its private, or if ends with simpleName prepended by two dollar signs,
        then let it be public, but look for whatever the Scala compiler puts in there to mark it as private at the Scala source level.

        // org$scalatest$FailureMessages$$decorateToStringValue
        // 0 org$scalatest$FailureMessages$$decorateToStringValue
        [java] 1 true
        [java] 2 false
        [java] false
        [java] false
        [java] ^&^&^&^&^&^& invalidArgs.length is: 0
        [java] 5 true

        println("0 "+ simpleName)
        println("1 "+ isInstanceMethod)
        println("2 "+ isPrivate)
        println("3 "+ simpleName == methodNameToInvoke)
        println("4 "+ candidateResultType == resultType)
        println("5 "+ argsHaveValidTypes)

        This ugliness. I'll ignore the result type for now. Sheesh. Investigate that one. And I'll
        have to ignore private too for now, because in the bytecodes it isn't even private. And I'll
        also allow methods that end with $$<simpleName> if the simpleName doesn't match
        */

        isInstanceMethod && (simpleName == methodNameToInvoke || simpleName.endsWith("$$"+ methodNameToInvoke)) && argsHaveValidTypes
      }
  
      // Store in an array, because may have both isEmpty and empty, in which case I
      // will throw an exception.
      val methodArray =
        for (m <- target.getClass.getDeclaredMethods; if isMethodToInvoke(m))
          yield m
  
      if (methodArray.length == 0)
        throw new IllegalArgumentException("Can't find a private method named: " + methodNameToInvoke)
      else if (methodArray.length > 1)
        throw new IllegalArgumentException("Found two methods")
      else {
        val anyRefArgs = // Need to box these myself, because that's invoke is expecting an Array[Object], which maps to an Array[AnyRef]
          for (arg <- args) yield arg match {
            case anyRef: AnyRef => anyRef
            case null => null
            case any => any.asInstanceOf[AnyRef]
          }
        val privateMethodToInvoke = methodArray(0)
        privateMethodToInvoke.setAccessible(true)
        try {
          privateMethodToInvoke.invoke(target, anyRefArgs.toArray: _*).asInstanceOf[T]
        }
        catch {
          case e: InvocationTargetException =>
            val cause = e.getCause
            if (cause != null) throw cause else throw e
        }
      }
    }
  }

  // SKIP-DOTTY-START
  import scala.language.implicitConversions

  /**
   * Implicit conversion from <code>AnyRef</code> to <code>Invoker</code>, used to enable
   * assertions testing of private methods.
   *
   * @param target the target object on which to invoke a private method.
   * @throws NullArgumentException if <code>target</code> is <code>null</code>.
   */
  implicit def anyRefToInvoker(target: AnyRef): Invoker = new Invoker(target)
  // SKIP-DOTTY-END

  //DOTTY-ONLY /**
  //DOTTY-ONLY  * Convert <code>AnyRef</code> to <code>Invoker</code>.
  //DOTTY-ONLY  *
  //DOTTY-ONLY  * @param target the target object on which to invoke a private method.
  //DOTTY-ONLY  * @throws NullArgumentException if <code>target</code> is <code>null</code>.
  //DOTTY-ONLY  */
  //DOTTY-ONLY def anyRefToInvoker(target: AnyRef): Invoker = new Invoker(target)

  //DOTTY-ONLY given Conversion[AnyRef, Invoker] with {
  //DOTTY-ONLY   def apply(target: AnyRef): Invoker = anyRefToInvoker(target)
  //DOTTY-ONLY }
}

/**
 * Companion object that facilitates the importing of <code>PrivateMethodTester</code> members as 
 * an alternative to mixing it in. One use case is to import <code>PrivateMethodTester</code> members so you can use
 * them in the Scala interpreter:
 *
 * <pre class="stREPL">
 * $scala -classpath scalatest.jar
 * Welcome to Scala version 2.7.5.final (Java HotSpot(TM) Client VM, Java 1.5.0_16).
 * Type in expressions to have them evaluated.
 * Type :help for more information.
 * &nbsp;
 * scala> import org.scalatest.PrivateMethodTester._                 
 * import org.scalatest.PrivateMethodTester._
 * &nbsp;
 * scala> class Example {
 *      |   private def addSesame(prefix: String) = prefix + " sesame"
 *      | }
 * defined class Example
 * &nbsp;
 * scala> val example = new Example
 * example: Example = Example@d8b6fe
 * &nbsp;
 * scala> val addSesame = PrivateMethod[String]('addSesame)           
 * addSesame: org.scalatest.PrivateMethodTester.PrivateMethod[String] = org.scalatest.PrivateMethodTester$PrivateMethod@5cdf95
 * &nbsp;
 * scala> example invokePrivate addSesame("open")                     
 * res0: String = open sesame
 * <pre>
 *
 * @author Bill Venners
 */
object PrivateMethodTester extends PrivateMethodTester

