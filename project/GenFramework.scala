import collection.GenTraversable
import scala.annotation.tailrec
import scala.util.matching.Regex

trait Template {
  val children: List[Template] = List.empty
  
  protected def childrenContent = 
    children.map(_.toString).map(_.split("\n").map("  " + _).mkString("\n")).mkString("\n") + "\n"
}

trait ClassTemplate extends Template {
  val name: String
  val extendName: Option[String] = None
  val withList: List[String] = List.empty
  
  private def getExtend = 
    extendName match {
      case Some(extendName) => "extends " + extendName
      case None => ""
    }
      
  private def getWith = 
    withList.map("with " + _).mkString(" ")
  
  override def toString = 
    "class " + name + " " + getExtend + " " + getWith + " {\n" + 
    childrenContent + 
    "}"
}

class ScalaFileTemplate(packageName: Option[String] = None, importList: List[String] = List.empty, definitionList: List[Template]) extends Template {
  
  override val children = definitionList
  
  private def getPackage = 
    packageName match {
      case Some(packageName) => "package " + packageName + "\n\n"
      case None => ""
    }
  
  private def getImports = 
    importList.map("import " + _).mkString("\n")
  
    
  override protected def childrenContent = 
    children.map(_.toString).map(_.split("\n").mkString("\n")).mkString("\n") + "\n"
    
  override def toString = 
    getPackage + 
    getImports + "\n" + 
    childrenContent
}

class SingleClassFile(packageName: Option[String] = None, importList: List[String] = List.empty, classTemplate: ClassTemplate) 
  extends ScalaFileTemplate(packageName, importList, List(classTemplate))

class CompositeTemplate(templates: List[Template], combinator: String = "") extends Template {
  override def toString = templates.map(_.toString).mkString(combinator)
}

class SimpleTemplate(value: String) extends Template {
  override def toString: String = value
}

class InterceptWithCauseTemplate(declaration: String, assertion: String, fileName: String, errMessage: String, lineAdj:Int, causeFileName: String, causeErrMessage: String, causeLineAdj: Int) extends Template {

  override def toString: String =
    declaration + "\n" +
    "val e = intercept[exceptions.TestFailedException] {\n" +
    assertion.split("\n").map("  " + _).mkString("\n") + "\n" +
    "}\n" +
    "assert(e.failedCodeFileName == Some(\"" + fileName + "\"))\n" +
    "assert(e.failedCodeLineNumber == Some(thisLineNumber - " + lineAdj + "))\n" +
    "assert(e.message == Some(" + errMessage + "))\n" +
    "e.getCause match {\n" +
    "  case tfe: exceptions.TestFailedException =>\n" +
    "    assert(tfe.failedCodeFileName == Some(\"" + causeFileName + "\"))\n" +
    "    assert(tfe.failedCodeLineNumber == Some(thisLineNumber - " + causeLineAdj + "))\n" +
    "    assert(tfe.message == Some(" + causeErrMessage + "))\n" +
    "    assert(tfe.getCause == null)\n" +
    "  case other => fail(\"Expected cause to be TestFailedException, but got: \" + other)\n" +
    "}\n"

}

class InterceptWithNullCauseTemplate(declaration: String, assertion: String, fileName: String, errMessage: String, lineAdj:Int) extends Template {

  override def toString: String =
    declaration + "\n" +
      "val e = intercept[exceptions.TestFailedException] {\n" +
      assertion.split("\n").map("  " + _).mkString("\n") + "\n" +
      "}\n" +
      "assert(e.failedCodeFileName == Some(\"" + fileName + "\"))\n" +
      "assert(e.failedCodeLineNumber == Some(thisLineNumber - " + lineAdj + "))\n" +
      "assert(e.message == Some(" + errMessage + "))\n" +
      "assert(e.getCause == null)\n"
}

class MessageTemplate(autoQuoteString: Boolean) extends Template {
  def wrapStringIfNecessary(value: Any): String = 
    value match {
      case strValue: String if autoQuoteString => "\\\"" + strValue + "\\\""
      case other => other.toString
    }
}

class SimpleMessageTemplate(message: String, autoQuoteString: Boolean = true) extends MessageTemplate(autoQuoteString) {
  override def toString = message
}

abstract class LeftMessageTemplate(left: Any, autoQuoteString: Boolean = true) extends MessageTemplate(autoQuoteString) {
  val message: String
  override def toString = 
    left + message
}
            
abstract class LeftRightMessageTemplate(left: Any, right: Any, autoQuoteString: Boolean = true) extends LeftMessageTemplate(left, autoQuoteString) {
  val message: String
  override def toString =
    left + message + wrapStringIfNecessary(right)
}
            
class EqualedMessageTemplate(left: Any, right: Any, autoQuoteString: Boolean = true) extends LeftRightMessageTemplate(left, right, autoQuoteString) {
  val message = " equaled "
}
            
class DidNotEqualMessageTemplate(left: Any, right: Any, autoQuoteString: Boolean = true) extends LeftRightMessageTemplate(left, right, autoQuoteString) {
  val message = " did not equal "
}
            
class WasEqualToMessageTemplate(left: Any, right: Any, autoQuoteString: Boolean = true) extends LeftRightMessageTemplate(left, right, autoQuoteString) {
  val message = " was equal to "
}
            
class WasNotEqualToMessageTemplate(left: Any, right: Any, autoQuoteString: Boolean = true) extends LeftRightMessageTemplate(left, right, autoQuoteString) {
  val message = " was not equal to "
}
            
class WasLessThanMessageTemplate(left: Any, right: Any, autoQuoteString: Boolean = true) extends LeftRightMessageTemplate(left, right, autoQuoteString) {
  val message = " was less than "
}
            
class WasNotLessThanMessageTemplate(left: Any, right: Any, autoQuoteString: Boolean = true) extends LeftRightMessageTemplate(left, right, autoQuoteString) {
  val message = " was not less than "
}
            
class WasLessThanOrEqualToMessageTemplate(left: Any, right: Any, autoQuoteString: Boolean = true) extends LeftRightMessageTemplate(left, right, autoQuoteString) {
  val message = " was less than or equal to "
}
            
class WasNotLessThanOrEqualToMessageTemplate(left: Any, right: Any, autoQuoteString: Boolean = true) extends LeftRightMessageTemplate(left, right, autoQuoteString) {
  val message = " was not less than or equal to "
}
            
class WasGreaterThanMessageTemplate(left: Any, right: Any, autoQuoteString: Boolean = true) extends LeftRightMessageTemplate(left, right, autoQuoteString) {
  val message = " was greater than "
}
            
class WasNotGreaterThanMessageTemplate(left: Any, right: Any, autoQuoteString: Boolean = true) extends LeftRightMessageTemplate(left, right, autoQuoteString) {
  val message = " was not greater than "
}
            
class WasGreaterThanOrEqualToMessageTemplate(left: Any, right: Any, autoQuoteString: Boolean = true) extends LeftRightMessageTemplate(left, right, autoQuoteString) {
  val message = " was greater than or equal to "
}
            
class WasNotGreaterThanOrEqualToMessageTemplate(left: Any, right: Any, autoQuoteString: Boolean = true) extends LeftRightMessageTemplate(left, right, autoQuoteString) {
  val message = " was not greater than or equal to "
}
            
class WasNullMessageTemplate extends SimpleMessageTemplate("The reference was null")
            
class WasNotNullMessageTemplate(left: Any, autoQuoteString: Boolean = true) extends LeftMessageTemplate(left, autoQuoteString) {
  val message = " was not null"
}
            
class WasMessageTemplate(left: Any, right: Any, autoQuoteString: Boolean = true) extends LeftRightMessageTemplate(left, right, autoQuoteString) {
  val message = " was "
}
            
class WasNotMessageTemplate(left: Any, right: Any, autoQuoteString: Boolean = true) extends LeftRightMessageTemplate(left, right, autoQuoteString) {
  val message = " was not "
}
            
class WasAMessageTemplate(left: Any, right: Any, autoQuoteString: Boolean = true) extends LeftRightMessageTemplate(left, right, autoQuoteString) {
  val message = " was a "
}
            
class WasNotAMessageTemplate(left: Any, right: Any, autoQuoteString: Boolean = true) extends LeftRightMessageTemplate(left, right, autoQuoteString) {
  val message = " was not a "
}
            
class WasAnMessageTemplate(left: Any, right: Any, autoQuoteString: Boolean = true) extends LeftRightMessageTemplate(left, right, autoQuoteString) {
  val message = " was an "
}
            
class WasNotAnMessageTemplate(left: Any, right: Any, autoQuoteString: Boolean = true) extends LeftRightMessageTemplate(left, right, autoQuoteString) {
  val message = " was not an "
}
            
class WasTheSameInstanceAsMessageTemplate(left: Any, right: Any, autoQuoteString: Boolean = true) extends LeftRightMessageTemplate(left, right, autoQuoteString) {
  val message = " was the same instance as "
}
            
class WasNotTheSameInstanceAsMessageTemplate(left: Any, right: Any, autoQuoteString: Boolean = true) extends LeftRightMessageTemplate(left, right, autoQuoteString) {
  val message = " was not the same instance as "
}
            
class PropertyHadUnexpectedValueMessageTemplate(propertyName: String, expectedValue: Any, value: Any, target: Any, autoQuoteString: Boolean = true) extends MessageTemplate(autoQuoteString) {
  override def toString = 
    "The " + propertyName + " property had value " + wrapStringIfNecessary(value) + ", instead of its expected value " + wrapStringIfNecessary(expectedValue) + ", on object " + wrapStringIfNecessary(target)
}

class PropertyHadExpectedValueMessageTemplate(propertyName: String, expectedValue: Any, target: Any, autoQuoteString: Boolean = true) extends MessageTemplate(autoQuoteString) {
  override def toString = 
    "The " + propertyName + " property had its expected value " + wrapStringIfNecessary(expectedValue) + ", on object " + wrapStringIfNecessary(target) 
}

class HadLengthMessageTemplate(left: Any, right: Any, autoQuoteString: Boolean = true) extends LeftRightMessageTemplate(left, right, autoQuoteString) {
  val message = " had length "
}

class HadLengthInsteadOfExpectedLengthMessageTemplate(left: Any, leftLength: Any, right: Any, autoQuoteString: Boolean = true) extends LeftRightMessageTemplate(left, right, autoQuoteString) {
  val message = " had length " + leftLength + " instead of expected length "
}

class HadSizeMessageTemplate(left: Any, right: Any, autoQuoteString: Boolean = true) extends LeftRightMessageTemplate(left, right, autoQuoteString) {
  val message = " had size "
}

class HadSizeInsteadOfExpectedSizeMessageTemplate(left: Any, leftSize: Any, right: Any, autoQuoteString: Boolean = true) extends LeftRightMessageTemplate(left, right, autoQuoteString) {
  val message = " had size " + leftSize + " instead of expected size "
}

class DidNotStartWithSubstringMessageTemplate(left: Any, right: Any, autoQuoteString: Boolean = true) extends LeftRightMessageTemplate(left, right, autoQuoteString) {
  val message = " did not start with substring "
}

class StartedWithSubstringMessageTemplate(left: Any, right: Any, autoQuoteString: Boolean = true) extends LeftRightMessageTemplate(left, right, autoQuoteString) {
  val message = " started with substring "
}

class DidNotEndWithSubstringMessageTemplate(left: Any, right: Any, autoQuoteString: Boolean = true) extends LeftRightMessageTemplate(left, right, autoQuoteString) {
  val message = " did not end with substring "
}

class EndedWithSubstringMessageTemplate(left: Any, right: Any, autoQuoteString: Boolean = true) extends LeftRightMessageTemplate(left, right, autoQuoteString) {
  val message = " ended with substring "
}

class DidNotIncludeSubstringMessageTemplate(left: Any, right: Any, autoQuoteString: Boolean = true) extends LeftRightMessageTemplate(left, right, autoQuoteString) {
  val message = " did not include substring "
}

class IncludedSubstringMessageTemplate(left: Any, right: Any, autoQuoteString: Boolean = true) extends LeftRightMessageTemplate(left, right, autoQuoteString) {
  val message = " included substring "
}

class DidNotStartWithRegexMessageTemplate(left: Any, right: Regex, autoQuoteString: Boolean = true) extends LeftRightMessageTemplate(left, right, autoQuoteString) {
  val message = " did not start with a substring that matched the regular expression "
}

class StartedWithRegexMessageTemplate(left: Any, right: Regex, autoQuoteString: Boolean = true) extends LeftRightMessageTemplate(left, right, autoQuoteString) {
  val message = " started with a substring that matched the regular expression "
}

class DidNotEndWithRegexMessageTemplate(left: Any, right: Regex, autoQuoteString: Boolean = true) extends LeftRightMessageTemplate(left, right, autoQuoteString) {
  val message = " did not end with a substring that matched the regular expression "
}

class EndedWithRegexMessageTemplate(left: Any, right: Regex, autoQuoteString: Boolean = true) extends LeftRightMessageTemplate(left, right, autoQuoteString) {
  val message = " ended with a substring that matched the regular expression "
}

class DidNotIncludeRegexMessageTemplate(left: Any, right: Regex, autoQuoteString: Boolean = true) extends LeftRightMessageTemplate(left, right, autoQuoteString) {
  val message = " did not include substring that matched regex "
}

class IncludedRegexMessageTemplate(left: Any, right: Regex, autoQuoteString: Boolean = true) extends LeftRightMessageTemplate(left, right, autoQuoteString) {
  val message = " included substring that matched regex "
}

class DidNotFullyMatchRegexMessageTemplate(left: Any, right: Regex, autoQuoteString: Boolean = true) extends LeftRightMessageTemplate(left, right, autoQuoteString) {
  val message = " did not fully match the regular expression "
}

class FullyMatchRegexMessageTemplate(left: Any, right: Regex, autoQuoteString: Boolean = true) extends LeftRightMessageTemplate(left, right, autoQuoteString) {
  val message = " fully matched the regular expression "
}

class DidNotContainElementMessageTemplate(left: Any, right: Any, autoQuoteString: Boolean = true) extends LeftRightMessageTemplate(left, right, autoQuoteString) {
  val message = " did not contain element "
}

class ContainedElementMessageTemplate(left: Any, right: Any, autoQuoteString: Boolean = true) extends LeftRightMessageTemplate(left, right, autoQuoteString) {
  val message = " contained element "
}

class DidNotContainKeyMessageTemplate(left: Any, right: Any, autoQuoteString: Boolean = true) extends LeftRightMessageTemplate(left, right, autoQuoteString) {
  val message = " did not contain key "
}

class ContainedKeyMessageTemplate(left: Any, right: Any, autoQuoteString: Boolean = true) extends LeftRightMessageTemplate(left, right, autoQuoteString) {
  val message = " contained key "
}

class DidNotContainValueMessageTemplate(left: Any, right: Any, autoQuoteString: Boolean = true) extends LeftRightMessageTemplate(left, right, autoQuoteString) {
  val message = " did not contain value "
}

class ContainedValueMessageTemplate(left: Any, right: Any, autoQuoteString: Boolean = true) extends LeftRightMessageTemplate(left, right, autoQuoteString) {
  val message = " contained value "
}

object Generator {
  
  import java.io.{File, FileWriter, BufferedWriter}
  
  def getIndex[T](xs: GenTraversable[T], value: T): Int = {
    @tailrec
    def getIndexAcc[T](itr: Iterator[T], count: Int): Int = {
      if (itr.hasNext) {
        val next = itr.next
        if (next == value)
          count
        else
          getIndexAcc(itr, count + 1)
      }
      else
        -1
    }
    getIndexAcc(xs.toIterator, 0)
  }
  
  @tailrec
  final def getNext[T](itr: Iterator[T], predicate: T => Boolean): T = {
    val next = itr.next
    if (predicate(next))
      next
    else
      getNext(itr, predicate)
  }
  
  def getFirst[T](col: GenTraversable[T], predicate: T => Boolean): T = 
    getNext(col.toIterator, predicate)
  
  @tailrec
  final def getNextNot[T](itr: Iterator[T], predicate: T => Boolean): T = {
    val next = itr.next
    if (!predicate(next))
      next
    else
      getNextNot(itr, predicate)
  }
  
  def getFirstNot[T](col: GenTraversable[T], predicate: T => Boolean): T = 
    getNextNot(col.toIterator, predicate)
  
  def genFile(targetFile: File, template: ScalaFileTemplate) = {
    val content = template.toString
    val writer = new BufferedWriter(new FileWriter(targetFile))
    try {
      writer.write(content)
    }
    finally {
      writer.flush()
      writer.close()
    }
  }
}
class GenFramework
