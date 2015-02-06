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
   * Represent a private method, whose apply method returns an <code>Invocation</code> object that
   * records the name of the private method to invoke, and any arguments to pass to it when invoked.
   * The type parameter, <code>T</code>, is the return type of the private method.
   *
   * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
   * @throws NullPointerException if <code>methodName</code> is <code>null</code>
   */
  final class PrivateMethod[T] private (methodName: Symbol) {

    if (methodName == null)
      throw new NullPointerException("methodName was null")

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
   * Contains a factory method for instantiating <code>PrivateMethod</code> objects.
   */
  object PrivateMethod {

    /**
     * Construct a new <code>PrivateMethod</code> object with passed <code>methodName</code> symbol.
     * The type parameter, <code>T</code>, is the return type of the private method.
     *
     * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
     * @throws NullPointerException if <code>methodName</code> is <code>null</code>
     */
    def apply[T](methodName: Symbol) = new PrivateMethod[T](methodName)
  }

  /**
   * Class whose instances represent an invocation of a private method. Instances of this
   * class contain the name of the private method (<code>methodName</code>) and the arguments
   * to pass to it during the invocation (<code>args</code>).
   * The type parameter, <code>T</code>, is the return type of the private method.
   *
   * @param methodName a <code>Symbol</code> representing the name of the private method to invoke
   * @param args zero to many arguments to pass to the private method when invoked
   * @throws NullPointerException if <code>methodName</code> is <code>null</code>
   */
  final class Invocation[T](val methodName: Symbol, val args: Any*) {
    if (methodName == null)
      throw new NullPointerException
  }

  /**
   * Class used via an implicit conversion to enable private methods to be tested.
   */
  final class Invoker(target: AnyRef) {

    if (target == null)
      throw new NullPointerException
    
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
      val methodNameToInvoke = methodName.toString.substring(1)

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
              val anyRefArg = arg.asInstanceOf[AnyRef]
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
            case any: Any => any.asInstanceOf[AnyRef] // Can't use AnyVal in 2.8
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

  import scala.language.implicitConversions

  /**
   * Implicit conversion from <code>AnyRef</code> to <code>Invoker</code>, used to enable
   * assertions testing of private methods.
   *
   * @param target the target object on which to invoke a private method.
   * @throws NullPointerException if <code>target</code> is <code>null</code>.
   */
  implicit def anyRefToInvoker(target: AnyRef): Invoker = new Invoker(target)
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

