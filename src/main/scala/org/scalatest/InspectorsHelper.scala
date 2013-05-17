package org.scalatest

import scala.annotation.tailrec
import org.scalatest.exceptions.StackDepthExceptionHelper.getStackDepthFun
import Suite.indentLines
import scala.collection.GenTraversable

private[scalatest] object InspectorsHelper {
  
  def indentErrorMessages(messages: IndexedSeq[String]) = indentLines(1, messages)
  
  def getResourceNamePrefix(xs: GenTraversable[_]): String = 
    xs match {
      case _: collection.GenMap[_, _] => "forAssertionsGenMapMessage"
      case _ => "forAssertionsGenTraversableMessage"
    }
  
  def shouldPropagate(throwable: Throwable): Boolean = 
    throwable match {
      case _: exceptions.TestPendingException |
           _: exceptions.TestCanceledException => true
      case _ if Suite.anExceptionThatShouldCauseAnAbort(throwable) => true
      case _ => false
    }
  
  def createMessage(messageKey: String, t: Throwable, resourceNamePrefix: String): String = 
    t match {
      case sde: exceptions.StackDepthException => 
        sde.failedCodeFileNameAndLineNumberString match {
          case Some(failedCodeFileNameAndLineNumber) => 
            Resources(resourceNamePrefix + "WithStackDepth", messageKey, sde.getMessage, failedCodeFileNameAndLineNumber)
          case None => 
            Resources(resourceNamePrefix + "WithoutStackDepth", messageKey, sde.getMessage)
        }
      case _ =>
        Resources(resourceNamePrefix + "WithoutStackDepth", messageKey, if (t.getMessage != null) t.getMessage else "null")
    }
  
  def elementLabel(count: Int): String = 
    if (count > 1) Resources("forAssertionsElements", count.toString) else Resources("forAssertionsElement", count.toString)
  
  case class ForResult[T](passedCount: Int = 0, messageAcc: IndexedSeq[String] = IndexedSeq.empty, 
                                 passedElements: IndexedSeq[(Int, T)] = IndexedSeq.empty, failedElements: IndexedSeq[(Int, T, Throwable)] = IndexedSeq.empty)
  
  @tailrec
  def runFor[T](itr: Iterator[T], resourceNamePrefix: String, index:Int, result: ForResult[T], fun: T => Unit, stopFun: ForResult[_] => Boolean): ForResult[T] = {
    if (itr.hasNext) {
      val head = itr.next
      val newResult = 
        try {
          fun(head)
          result.copy(passedCount = result.passedCount + 1, passedElements = result.passedElements :+ (index, head))
        }
        catch {
          case e if !shouldPropagate(e) => 
            val messageKey = head match {
              case tuple: Tuple2[_, _] if resourceNamePrefix == "forAssertionsGenMapMessage" => tuple._1.toString
              case _ => index.toString
            }
            result.copy(messageAcc = result.messageAcc :+ createMessage(messageKey, e, resourceNamePrefix), failedElements = result.failedElements :+ (index, head, e))
        }
      if (stopFun(newResult))
        newResult
      else
        runFor(itr, resourceNamePrefix, index + 1, newResult, fun, stopFun)
    }
    else
      result
  }
  
  def keyOrIndexLabel(xs: GenTraversable[_], passedElements: IndexedSeq[(Int, _)]): String = {
    def makeAndLabel(indexes: IndexedSeq[Int]): String = 
      if (indexes.length > 1)
        indexes.dropRight(1).mkString(", ") + " and " + indexes.last
      else
        indexes.mkString(", ")
      
    val (prefixResourceName, elements) = xs match {
      case map: collection.GenMap[_, _] => 
        val elements = passedElements.map{ case (index, e) => 
          e match {
            case tuple2: Tuple2[_, _] => tuple2._1
            case _ => index
          }
        }
        ("forAssertionsKey", elements)
      case _ => 
        ("forAssertionsIndex", passedElements.map(_._1))
    }
    
    if (elements.length > 1)
      Resources(prefixResourceName + "AndLabel", elements.dropRight(1).mkString(", "), elements.last.toString) 
    else
      Resources(prefixResourceName + "Label", elements.mkString(", "))
  }
  
  def doForAll[T](xs: GenTraversable[T], resourceName: String, sourceFileName: String, methodName: String, stackDepthAdjustment: Int)(fun: T => Unit) {
    val resourceNamePrefix = getResourceNamePrefix(xs)
    val result = 
      runFor(xs.toIterator, resourceNamePrefix, 0, new ForResult[T], fun, _.failedElements.length > 0)
    if (result.failedElements.length > 0) 
      throw new exceptions.TestFailedException(
        sde => Some(Resources(resourceName, indentErrorMessages(result.messageAcc).mkString(", \n"), xs.toString)),
        Some(result.failedElements(0)._3),
        getStackDepthFun(sourceFileName, methodName, stackDepthAdjustment)
      )
  }
  
  def doForAtLeast[T](min: Int, xs: GenTraversable[T], resourceName: String, sourceFileName: String, methodName: String, stackDepthAdjustment: Int)(fun: T => Unit) {
    @tailrec
    def forAtLeastAcc(itr: Iterator[T], includeIndex: Boolean, index: Int, passedCount: Int, messageAcc: IndexedSeq[String]): (Int, IndexedSeq[String]) = {
      if (itr.hasNext) {
        val head = itr.next
        val (newPassedCount, newMessageAcc) = 
          try {
            fun(head)
            (passedCount + 1, messageAcc)
          }
          catch {
            case e if !shouldPropagate(e) => 
              val resourceNamePrefix = getResourceNamePrefix(xs)
              val messageKey = head match {
                case tuple: Tuple2[_, _] if resourceNamePrefix == "forAssertionsGenMapMessage" => tuple._1.toString
                case _ => index.toString
              }
              (passedCount, messageAcc :+ createMessage(messageKey, e, resourceNamePrefix))
          }
        if (newPassedCount < min)
          forAtLeastAcc(itr, includeIndex, index + 1, newPassedCount, newMessageAcc)
        else
          (newPassedCount, newMessageAcc)
      }
      else
        (passedCount, messageAcc)
    }
    
    if (min <= 0)
      throw new IllegalArgumentException(Resources("forAssertionsMoreThanZero", "'min'"))
    
    val (passedCount, messageAcc) = forAtLeastAcc(xs.toIterator, xs.isInstanceOf[Seq[T]], 0, 0, IndexedSeq.empty)
    if (passedCount < min)
      throw new exceptions.TestFailedException(
        sde => 
          Some(
            if (passedCount > 0)
              Resources(resourceName, min.toString, elementLabel(passedCount), indentErrorMessages(messageAcc).mkString(", \n"), xs.toString)
            else
              Resources(resourceName + "NoElement", min.toString, indentErrorMessages(messageAcc).mkString(", \n"), xs.toString)
          ),
        None,
        getStackDepthFun(sourceFileName, methodName, stackDepthAdjustment)
      )
  }
  
  def doForEvery[T](xs: GenTraversable[T], resourceName: String, sourceFileName: String, methodName: String, stackDepthAdjustment: Int)(fun: T => Unit) {
    @tailrec
    def runAndCollectErrorMessage[T](itr: Iterator[T], messageList: IndexedSeq[String], index: Int)(fun: T => Unit): IndexedSeq[String] = {
      if (itr.hasNext) {
        val head = itr.next
        val newMessageList = 
          try {
            fun(head)
            messageList
          }
          catch {
            case e if !shouldPropagate(e) => 
              val resourceNamePrefix = getResourceNamePrefix(xs)
              val messageKey = head match {
                case tuple: Tuple2[_, _] if resourceNamePrefix == "forAssertionsGenMapMessage" => tuple._1.toString
                case _ => index.toString
              }
              messageList :+ createMessage(messageKey, e, resourceNamePrefix)
          }
        
        runAndCollectErrorMessage(itr, newMessageList, index + 1)(fun)
      }
      else
        messageList
    }
    val messageList = runAndCollectErrorMessage(xs.toIterator, IndexedSeq.empty, 0)(fun)
    if (messageList.size > 0)
      throw new exceptions.TestFailedException(
          sde => Some(Resources(resourceName, indentErrorMessages(messageList).mkString(", \n"), xs)),
          None,
          getStackDepthFun(sourceFileName, methodName, stackDepthAdjustment)
        )
  }
  
  def doForExactly[T](succeededCount: Int, xs: GenTraversable[T], resourceName: String, sourceFileName: String, methodName: String, stackDepthAdjustment: Int)(fun: T => Unit) {
    if (succeededCount <= 0)
      throw new IllegalArgumentException(Resources("forAssertionsMoreThanZero", "'succeededCount'"))
    
    val resourceNamePrefix = getResourceNamePrefix(xs)
    val result = 
      runFor(xs.toIterator, resourceNamePrefix, 0, new ForResult[T], fun, _.passedCount > succeededCount)
    if (result.passedCount != succeededCount)
      throw new exceptions.TestFailedException(
        sde => 
          Some(
            if (result.passedCount == 0)
              Resources(resourceName + "NoElement", succeededCount.toString, indentErrorMessages(result.messageAcc).mkString(", \n"), xs.toString)
            else {
              if (result.passedCount < succeededCount)
                Resources(resourceName + "Less", succeededCount.toString, elementLabel(result.passedCount), keyOrIndexLabel(xs, result.passedElements), indentErrorMessages(result.messageAcc).mkString(", \n"), xs.toString)
              else
                Resources(resourceName + "More", succeededCount.toString, elementLabel(result.passedCount), keyOrIndexLabel(xs, result.passedElements), xs.toString)
            }
          ),
        None,
        getStackDepthFun(sourceFileName, methodName, stackDepthAdjustment)
      )
  }

  def doForNo[T](xs: GenTraversable[T], resourceName: String, sourceFileName: String, methodName: String, stackDepthAdjustment: Int)(fun: T => Unit) {
    val resourceNamePrefix = getResourceNamePrefix(xs)
    val result =
      runFor(xs.toIterator, resourceNamePrefix, 0, new ForResult[T], fun, _.passedCount != 0)
    if (result.passedCount != 0)
      throw new exceptions.TestFailedException(
        sde => Some(Resources(resourceName, keyOrIndexLabel(xs, result.passedElements), xs)),
        None,
        getStackDepthFun(sourceFileName, methodName, stackDepthAdjustment)
      )
  }

  def doForBetween[T](from: Int, upTo: Int, xs: GenTraversable[T], resourceName: String, sourceFileName: String, methodName: String, stackDepthAdjustment: Int)(fun: T => Unit) {
    if (from < 0)
      throw new IllegalArgumentException(Resources("forAssertionsMoreThanEqualZero", "'from'"))
    if (upTo <= 0)
      throw new IllegalArgumentException(Resources("forAssertionsMoreThanZero", "'upTo'"))
    if (upTo <= from)
      throw new IllegalArgumentException(Resources("forAssertionsMoreThan", "'upTo'", "'from'"))

    val resourceNamePrefix = getResourceNamePrefix(xs)
    val result =
      runFor(xs.toIterator, resourceNamePrefix, 0, new ForResult[T], fun, _.passedCount > upTo)
    if (result.passedCount < from || result.passedCount > upTo)
      throw new exceptions.TestFailedException(
        sde =>
          Some(
            if (result.passedCount == 0)
              Resources(resourceName + "NoElement", from.toString, upTo.toString, indentErrorMessages(result.messageAcc).mkString(", \n"), xs)
            else {
              if (result.passedCount < from)
                Resources(resourceName + "Less", from.toString, upTo.toString, elementLabel(result.passedCount), keyOrIndexLabel(xs, result.passedElements), indentErrorMessages(result.messageAcc).mkString(", \n"), xs)
              else
                Resources(resourceName + "More", from.toString, upTo.toString, elementLabel(result.passedCount), keyOrIndexLabel(xs, result.passedElements), xs)
            }
          ),
        None,
        getStackDepthFun(sourceFileName, methodName, stackDepthAdjustment)
      )
  }

  def doForAtMost[T](max: Int, xs: GenTraversable[T], resourceName: String, sourceFileName: String, methodName: String, stackDepthAdjustment: Int)(fun: T => Unit) {
    if (max <= 0)
      throw new IllegalArgumentException(Resources("forAssertionsMoreThanZero", "'max'"))

    val resourceNamePrefix = getResourceNamePrefix(xs)
    val result =
      runFor(xs.toIterator, resourceNamePrefix, 0, new ForResult[T], fun, _.passedCount > max)
    if (result.passedCount > max)
      throw new exceptions.TestFailedException(
        sde => Some(Resources(resourceName, max.toString, result.passedCount.toString, keyOrIndexLabel(xs, result.passedElements), xs.toString)),
        None,
        getStackDepthFun(sourceFileName, methodName, stackDepthAdjustment)
      )
  }
}