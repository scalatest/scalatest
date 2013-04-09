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
package org.scalatest.exceptions

/**
 * Exception class that encapsulates information about the stack depth at which the line of code that failed resides,
 * so that information can be presented to the user that makes it quick to find the failing line of code. (In other
 * words, the user need not scan through the stack trace to find the correct filename and line number of the problem code.)
 * Having a stack depth is more useful in a testing environment in which test failures are implemented as
 * thrown exceptions, as is the case in ScalaTest's built-in suite traits.
 *
 * @param messageFun an function that produces an optional detail message for this <code>StackDepthException</code>.
 * @param cause an optional cause, the <code>Throwable</code> that caused this <code>StackDepthException</code> to be thrown.
 * @param failedCodeStackDepthFun a function that produces the depth in the stack trace of this exception at which the line of test code that failed resides.
 *
 * @throws NullPointerException if either <code>messageFun</code>, <code>cause</code> or <code>failedCodeStackDepthFun</code> is <code>null</code>, or <code>Some(null)</code>.
 *
 * @author Bill Venners
 */
abstract class StackDepthException(
  val messageFun: StackDepthException => Option[String],
  val cause: Option[Throwable],
  val failedCodeStackDepthFun: StackDepthException => Int
) extends RuntimeException(if (cause.isDefined) cause.get else null) with StackDepth {

  if (messageFun == null) throw new NullPointerException("messageFun was null")

  if (cause == null) throw new NullPointerException("cause was null")
  cause match {
    case Some(null) => throw new NullPointerException("cause was a Some(null)")
    case _ =>
  }

  if (failedCodeStackDepthFun == null) throw new NullPointerException("failedCodeStackDepthFun was null")

  /**
   * Constructs a <code>StackDepthException</code> with an optional pre-determined <code>message</code>, optional cause, and
   * a <code>failedCodeStackDepth</code> function.
   *
   * @param message an optional detail message for this <code>StackDepthException</code>.
   * @param cause an optional cause, the <code>Throwable</code> that caused this <code>StackDepthException</code> to be thrown.
   * @param failedCodeStackDepth the depth in the stack trace of this exception at which the line of test code that failed resides.
   *
   * @throws NullPointerException if either <code>message</code> or <code>cause</code> is <code>null</code> or <code>Some(null)</code>, or <code>failedCodeStackDepthFun</code> is <code>null</code>.
   */
  def this(message: Option[String], cause: Option[Throwable], failedCodeStackDepthFun: StackDepthException => Int) =
    this(
      message match {
        case null => throw new NullPointerException("message was null")
        case Some(null) => throw new NullPointerException("message was a Some(null)")
        case _ => (e: StackDepthException) => message
      },
      cause,
      failedCodeStackDepthFun
    )

  /**
   * Constructs a <code>StackDepthException</code> with an optional pre-determined <code>message</code>,
   * optional <code>cause</code>, and and <code>failedCodeStackDepth</code>. (This was
   * the primary constructor form prior to ScalaTest 1.5.)
   *
   * @param message an optional detail message for this <code>StackDepthException</code>.
   * @param cause an optional cause, the <code>Throwable</code> that caused this <code>StackDepthException</code> to be thrown.
   * @param failedCodeStackDepth the depth in the stack trace of this exception at which the line of test code that failed resides.
   *
   * @throws NullPointerException if either <code>message</code> of <code>cause</code> is <code>null</code>, or <code>Some(null)</code>.
   */
  def this(message: Option[String], cause: Option[Throwable], failedCodeStackDepth: Int) =
    this(
      message match {
        case null => throw new NullPointerException("message was null")
        case Some(null) => throw new NullPointerException("message was a Some(null)")
        case _ => (e: StackDepthException) => message
      },
      cause,
      (e: StackDepthException) => failedCodeStackDepth
    )

  /**
   * An optional detail message for this <code>StackDepth</code> exception.
   *
   * <p>
   * One reason this is lazy is to delay any searching of the stack trace until it is actually needed. It will
   * usually be needed, but not always. For example, exceptions thrown during a shrink phase of a failed property
   * will often be <code>StackDepthException</code>s, but whose <code>message</code> will never be used. Another related reason is to remove the need
   * to create a different exception before creating this one just for the purpose of searching through its stack
   * trace for the proper stack depth. Still one more reason is to allow the message to contain information about the
   * stack depth, such as the failed file name and line number.
   * </p>
   */
  lazy val message: Option[String] = messageFun(this)
 
  /**
   * The depth in the stack trace of this exception at which the line of test code that failed resides.
   *
   * <p>
   * One reason this is lazy is to delay any searching of the stack trace until it is actually needed. It will
   * usually be needed, but not always. For example, exceptions thrown during a shrink phase of a failed property
   * will often be <code>StackDepthException</code>s, but whose <code>failedCodeStackDepth</code> will never be used. Another reason is to remove the need
   * to create a different exception before creating this one just for the purpose of searching through its stack
   * trace for the proper stack depth. Still one more reason is to allow the message to contain information about the
   * stack depth, such as the failed file name and line number.
   * </p>
   */
  lazy val failedCodeStackDepth: Int = failedCodeStackDepthFun(this)

  /**
   * Returns the detail message string of this <code>StackDepthException</code>.
   *
   * @return the detail message string of this <code>StackDepthException</code> instance (which may be <code>null</code>).
   */
  override def getMessage: String = message.orNull

  /*
  * Throws <code>IllegalStateException</code>, because <code>StackDepthException</code>s are
  * always initialized with a cause passed to the constructor of superclass <code>
  */
  override final def initCause(throwable: Throwable): Throwable = { throw new IllegalStateException }

  /**
   * Indicates whether this object can be equal to the passed object.
   */
  def canEqual(other: Any): Boolean = other.isInstanceOf[StackDepthException]

  /**
   * Indicates whether this object is equal to the passed object. If the passed object is
   * a <code>StackDepthException</code>, equality requires equal <code>message</code>,
   * <code>cause</code>, and <code>failedCodeStackDepth</code> fields, as well as equal
   * return values of <code>getStackTrace</code>.
   */
  override def equals(other: Any): Boolean =
    other match {
      case that: StackDepthException => 
        (that canEqual this) &&
        message == that.message &&
        cause == that.cause &&
        failedCodeStackDepth == that.failedCodeStackDepth &&
        getStackTrace.deep == that.getStackTrace.deep
      case _ => false
    }

  /**
   * Returns a hash code value for this object.
   */
  override def hashCode: Int =
    41 * (
      41 * (
        41 * (
          41 + message.hashCode
        ) + cause.hashCode
      ) + failedCodeStackDepth.hashCode
    ) + getStackTrace.hashCode
}

/*
For check methods in Checkers, passed fileName will be "Checkers.scala" and
passed methodName will be "check":

0 org.scalatest.prop.Checkers$class.check(Checkers.scala:194)
1 org.scalatest.ShouldContainElementSpec.check(ShouldContainElementSpec.scala:23)
2 org.scalatest.prop.Checkers$class.check(Checkers.scala:205)
3 org.scalatest.ShouldContainElementSpec.check(ShouldContainElementSpec.scala:23)
4 org.scalatest.prop.Checkers$class.check(Checkers.scala:96)
5 org.scalatest.ShouldContainElementSpec.check(ShouldContainElementSpec.scala:23)
6 org.scalatest.ShouldContainElementSpec$$anonfun$1$$anonfun$apply$1$$anonfun$apply$28.apply(ShouldContainElementSpec.scala:80)

For detection of a duplicate test name in Spec, passed fileName will be "Spec.scala" and
passed methodName will be "it":

0 org.scalatest.FunSpec$class.registerExample(Spec.scala:682)
1 org.scalatest.FunSpec$class.it(Spec.scala:712)
2 org.scalatest.ShouldContainElementSpec.it(ShouldContainElementSpec.scala:23)
3 org.scalatest.FunSpec$class.it(Spec.scala:735)
4 org.scalatest.ShouldContainElementSpec.it(ShouldContainElementSpec.scala:23)
5 org.scalatest.ShouldContainElementSpec$$anonfun$1$$anonfun$apply$167.apply(ShouldContainElementSpec.scala:1092) 

For detection of a duplicate test name in FunSuite, passed fileName will be "FunSuite.scala" and
passed methodName will be "test":

0 org.scalatest.FunSuite$class.test(FunSuite.scala:592)
1 org.scalatest.SpecSuite.test(SpecSuite.scala:18)
2 org.scalatest.SpecSuite.<init>(SpecSuite.scala:42) 

For detection of an it inside an it in Spec, passed fileName will be "Spec.scala" and
passed methodName will be "it":

0 org.scalatest.FunSpec$class.it(Spec.scala:745)
1 org.scalatest.ShouldBehaveLikeSpec.it(ShouldBehaveLikeSpec.scala:23)
2 org.scalatest.ShouldBehaveLikeSpec$$anonfun$1.apply(ShouldBehaveLikeSpec.scala:26) 

For detection of a describe inside an it in Spec, passed fileName will be "Spec.scala" and
passed methodName will be "describe":

0 org.scalatest.FunSpec$class.describe(Spec.scala:804)
1 org.scalatest.ShouldBehaveLikeSpec.describe(ShouldBehaveLikeSpec.scala:23)
2 org.scalatest.ShouldBehaveLikeSpec$$anonfun$1.apply(ShouldBehaveLikeSpec.scala:26) 

For detection of an ignore inside an it in Spec, passed fileName will be "Spec.scala" and
passed methodName will be "ignore":

0 org.scalatest.FunSpec$class.ignore(Spec.scala:792)
1 org.scalatest.ShouldBehaveLikeSpec.ignore(ShouldBehaveLikeSpec.scala:23)
2 org.scalatest.ShouldBehaveLikeSpec$$anonfun$1.apply(ShouldBehaveLikeSpec.scala:26) 

For detection of a test inside a test in FunSuite, passed fileName will be "FunSuite.scala" and
passed methodName will be "test":

0 org.scalatest.FunSuite$class.test(FunSuite.scala:591)
1 org.scalatest.Q36Suite.test(ShouldBehaveLikeSpec.scala:23)
2 org.scalatest.Q36Suite$$anonfun$1.apply(ShouldBehaveLikeSpec.scala:25)

For detection of an ignore inside a test in FunSuite, passed fileName will be "FunSuite.scala" and
passed methodName will be "ignore":

0 org.scalatest.FunSuite$class.ignore(FunSuite.scala:624)
1 org.scalatest.Q36Suite.ignore(ShouldBehaveLikeSpec.scala:23)
2 org.scalatest.Q36Suite$$anonfun$1.apply(ShouldBehaveLikeSpec.scala:25)

Not sure yet what to do with TableDrivenPropertyCheckFailedExeptions. It seems to 
work fine hard-coded at 7. Can't find a case that doesn't work. Will release it hard-coded at 7 and see
if someone else runs across one, and if so, I'll fix it then. (So the code that throws that exception
doesn't call the getStackDepth helper method at this point.)

0 org.scalatest.prop.TableFor2$$anonfun$apply$4.apply(Table.scala:356)
1 org.scalatest.prop.TableFor2$$anonfun$apply$4.apply(Table.scala:347)
2 scala.collection.mutable.ResizableArray$class.foreach(ResizableArray.scala:57)
3 scala.collection.mutable.ArrayBuffer.foreach(ArrayBuffer.scala:43)
4 org.scalatest.prop.TableFor2.apply(Table.scala:347)
5 org.scalatest.prop.TableDrivenPropertyChecks$class.forAll(TableDrivenPropertyChecks.scala:215)
6 org.scalatest.prop.PropertyChecksSuite.forAll(PropertyChecksSuite.scala:21)
org.scalatest.prop.PropertyChecksSuite$$anonfun$2.apply(PropertyChecksSuite.scala:48) <-- this should not be cut

Conductor from conduct method: Stack depth should be 3 or 4. Both of which are the same

[scalatest] org.scalatest.NotAllowedException: A Conductor's conduct method can only be invoked once.
[scalatest] 	at org.scalatest.concurrent.Conductor.conduct(Conductor.scala:525)
[scalatest] 	at org.scalatest.concurrent.Conductor.conduct(Conductor.scala:476)
[scalatest] 	at org.scalatest.concurrent.ConductorSuite$$anonfun$1$$anonfun$2.apply(ConductorSuite.scala:30)
[scalatest] 	at org.scalatest.concurrent.ConductorSuite$$anonfun$1$$anonfun$2.apply(ConductorSuite.scala:30)
[scalatest] 	at org.scalatest.Assertions$class.intercept(Assertions.scala:515)
[scalatest] 	at org.scalatest.concurrent.ConductorSuite.intercept(ConductorSuite.scala:23)
[scalatest] 	at org.scalatest.concurrent.ConductorSuite$$anonfun$1.apply(ConductorSuite.scala:30)
[scalatest] 	at org.scalatest.concurrent.ConductorSuite$$anonfun$1.apply(ConductorSuite.scala:27)
[scalatest] 	at org.scalatest.FunSuite$$anon$1.apply(FunSuite.scala:1031)
[scalatest] 	at org.scalatest.Suite$class.withFixture(Suite.scala:1450)
[scalatest] 	at org.scalatest.concurrent.ConductorSuite.withFixture(ConductorSuite.scala:23)
[scalatest] 	at org.scalatest.FunSuite$class.runTest(FunSuite.scala:1028)
[scalatest] 	at org.scalatest.concurrent.ConductorSuite.runTest(ConductorSuite.scala:23)
*/
private[scalatest] object StackDepthExceptionHelper {
  
  def getStackDepth(stackTrace: Array[StackTraceElement], fileName: String, methodName: String, adjustment: Int = 0) = {
    val stackTraceList = stackTrace.toList

    val fileNameIsDesiredList: List[Boolean] =
      for (element <- stackTraceList) yield
        element.getFileName == fileName // such as "Checkers.scala"

    val methodNameIsDesiredList: List[Boolean] =
      for (element <- stackTraceList) yield
        element.getMethodName == methodName // such as "check"

    // For element 0, the previous file name was not desired, because there is no previous
    // one, so you start with false. For element 1, it depends on whether element 0 of the stack trace
    // had the desired file name, and so forth.
    val previousFileNameIsDesiredList: List[Boolean] = false :: (fileNameIsDesiredList.dropRight(1))

    // Zip these two related lists together. They now have two boolean values together, when both
    // are true, that's a stack trace element that should be included in the stack depth.
    val zipped1 = methodNameIsDesiredList zip previousFileNameIsDesiredList
    val methodNameAndPreviousFileNameAreDesiredList: List[Boolean] =
      for ((methodNameIsDesired, previousFileNameIsDesired) <- zipped1) yield
        methodNameIsDesired && previousFileNameIsDesired

    // Zip the two lists together, that when one or the other is true is an include.
    val zipped2 = fileNameIsDesiredList zip methodNameAndPreviousFileNameAreDesiredList
    val includeInStackDepthList: List[Boolean] =
      for ((fileNameIsDesired, methodNameAndPreviousFileNameAreDesired) <- zipped2) yield
        fileNameIsDesired || methodNameAndPreviousFileNameAreDesired

    val includeDepth = includeInStackDepthList.takeWhile(include => include).length
    val depth = if (includeDepth == 0 && stackTrace(0).getFileName != fileName && stackTrace(0).getMethodName != methodName) 
      stackTraceList.takeWhile(st => st.getFileName != fileName || st.getMethodName != methodName).length
    else
      includeDepth
    
    depth + adjustment
  }
  
  def getStackDepthFun(fileName: String, methodName: String, adjustment: Int = 0): (StackDepthException => Int) = { sde =>
    getStackDepth(sde.getStackTrace, fileName, methodName, adjustment)
  }
}

private[scalatest] object StackDepthException {

  /**
   * If message or message contents are null, throw a null exception, otherwise
   * create a function that returns the option.
   */
  def toExceptionFunction(message: Option[String]): StackDepthException => Option[String] = {
    message match {
        case null => throw new NullPointerException("message was null")
        case Some(null) => throw new NullPointerException("message was a Some(null)")
        case _ => { e => message }
    }
  }
}

