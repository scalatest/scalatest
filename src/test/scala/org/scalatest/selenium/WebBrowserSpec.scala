/*
 * Copyright 2001-2012 Artima, Inc.
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
package org.scalatest.selenium

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import java.util.concurrent.TimeUnit
import org.scalatest.time.SpanSugar
import org.scalatest.ParallelTestExecution
import org.openqa.selenium.WebDriver
import org.openqa.selenium.htmlunit.HtmlUnitDriver
import org.scalatest.time.Span
import org.scalatest.time.Seconds
import org.scalatest.exceptions.TestFailedException
import java.io.File
import org.scalatest.Suite
import org.scalatest.Args
import org.scalatest.ScreenshotOnFailure
import org.scalatest.SharedHelpers.SilentReporter
import org.scalatest.tagobjects.Slow
import org.openqa.selenium.firefox.FirefoxDriver
import org.openqa.selenium.firefox.FirefoxProfile
import org.openqa.selenium.safari.SafariDriver
import org.openqa.selenium.chrome.ChromeDriver
import org.openqa.selenium.ie.InternetExplorerDriver
import org.openqa.selenium.Cookie
import org.openqa.selenium.WebElement

class WebBrowserSpec extends JettySpec with ShouldMatchers with SpanSugar with WebBrowser with HtmlUnit {

  describe("textField") {
    it("should throw TFE with valid stack depth if specified item not found") {
      go to (host + "find-textfield.html")
      val caught = intercept[TestFailedException] {
        textField("unknown")
      }
      caught.failedCodeLineNumber should be (Some(thisLineNumber - 2))
      caught.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
    }
    it("should throw TFE with valid stack depth if specified is found but is not a text field") {
      go to (host + "find-textfield.html")
      val caught = intercept[TestFailedException] {
        textField("area1")
      }
      caught.failedCodeLineNumber should be (Some(thisLineNumber - 2))
      caught.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
    }
    it("should, when a valid text field is found, return a TestField instance") {
      go to (host + "find-textfield.html")
      val text1 = textField("text1")
      text1.value should be ("value1")
    }
    it("should, when multiple matching text fields exist, return the first one") {
      go to (host + "find-textfield.html")
      val text2 = textField("text2")
      text2.value should be ("value2")
    }
  }
  
  describe("pwdField") {
    it("should throw TFE with valid stack depth if specified item not found") {
      go to (host + "find-pwdfield.html")
      val caught = intercept[TestFailedException] {
        pwdField("unknown")
      }
      caught.failedCodeLineNumber should be (Some(thisLineNumber - 2))
      caught.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
    }
    it("should throw TFE with valid stack depth if specified is found but is not a password field") {
      go to (host + "find-pwdfield.html")
      val caught = intercept[TestFailedException] {
        pwdField("area1")
      }
      caught.failedCodeLineNumber should be (Some(thisLineNumber - 2))
      caught.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
    }
    it("should, when a valid password field is found, return a PasswordField instance") {
      go to (host + "find-pwdfield.html")
      val secret1 = pwdField("secret1")
      secret1.value should be ("pwd1")
    }
    it("should, when multiple matching password fields exist, return the first one") {
      go to (host + "find-pwdfield.html")
      val secret2 = pwdField("secret2")
      secret2.value should be ("pwd2")
    }
  }

  describe("textArea") {
    it("should throw TFE with valid stack depth if specified item not found") {
      go to (host + "find-textarea.html")
      val caught = intercept[TestFailedException] {
        textArea("unknown")
      }
      caught.failedCodeLineNumber should be (Some(thisLineNumber - 2))
      caught.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
    }
    it("should throw TFE with valid stack depth if specified is found but is not a text area") {
      go to (host + "find-textarea.html")
      val caught = intercept[TestFailedException] {
        textArea("opt1")
      }
      caught.failedCodeLineNumber should be (Some(thisLineNumber - 2))
      caught.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
    }
    it("should, when a valid text area is found, return a TestArea instance") {
      go to (host + "find-textarea.html")
      val textarea1 = textArea("textarea1")
      textarea1.text should be ("value1")
    }
    it("should, when multiple matching text areas exist, return the first one") {
      go to (host + "find-textarea.html")
      val text2 = textArea("textarea2")
      text2.text should be ("value2")
    }
  }

  describe("radioButton") {
    it("should throw TFE with valid stack depth if specified item not found") {
      go to (host + "find-radio.html")
      val caught = intercept[TestFailedException] {
        radioButton("unknown")
      }
      caught.failedCodeLineNumber should be (Some(thisLineNumber - 2))
      caught.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
    }
    it("should throw TFE with valid stack depth if specified is found but is not a radio button") {
      go to (host + "find-radio.html")
      val caught = intercept[TestFailedException] {
        radioButton("text1")
      }
      caught.failedCodeLineNumber should be (Some(thisLineNumber - 2))
      caught.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
    }
    it("should, when a valid radio button is found, return a RadioButton instance") {
      go to (host + "find-radio.html")
      val radio = radioButton("group1")
      radio.value should be ("value1")
    }
    it("should, when multiple matching radio buttons exist, return the first one") {
      go to (host + "find-radio.html")
      val radio = radioButton("group2")
      radio.value should be ("value2")
    }
  }

  describe("checkbox") {
    it("should throw TFE with valid stack depth if specified item not found") {
      go to (host + "find-checkbox.html")
      val caught = intercept[TestFailedException] {
        checkbox("unknown")
      }
      caught.failedCodeLineNumber should be (Some(thisLineNumber - 2))
      caught.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
    }
    it("should throw TFE with valid stack depth if specified is found but is not a checkbox") {
      go to (host + "find-checkbox.html")
      val caught = intercept[TestFailedException] {
        checkbox("text1")
      }
      caught.failedCodeLineNumber should be (Some(thisLineNumber - 2))
      caught.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
    }
    it("should, when a valid text field is found, return a Checkbox instance") {
      go to (host + "find-checkbox.html")
      val checkbox1 = checkbox("opt1")
      checkbox1.isSelected should be (true)
    }
    it("should, when multiple matching checkboxes exist, return the first one") {
      go to (host + "find-checkbox.html")
      val checkbox2 = checkbox("opt2")
      checkbox2.isSelected should be (false)
    }
  }

  describe("singleSel") {
    it("should throw TFE with valid stack depth if specified item not found") {
      go to (host + "find-select.html")
      val caught = intercept[TestFailedException] {
        singleSel("unknown")
      }
      caught.failedCodeLineNumber should be (Some(thisLineNumber - 2))
      caught.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
    }
    it("should throw TFE with valid stack depth if specified is found but is not a single-selection list") {
      go to (host + "find-select.html")
      val caught = intercept[TestFailedException] {
        singleSel("select2")
      }
      caught.failedCodeLineNumber should be (Some(thisLineNumber - 2))
      caught.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
    }
    it("should, when a valid single-selection list is found, return a SingleSel instance") {
      go to (host + "find-select.html")
      val select1 = singleSel("select1")
      select1.value should be ("option2")
    }
    it("should, when multiple matching single-selection lists exist, return the first one") {
      go to (host + "find-select.html")
      val select3 = singleSel("select3")
      select3.value should be ("option3")
    }
    it("should throw TFE with valid stack depth and cause if invalid option is set") {
      go to (host + "find-select.html")
      val select1 = singleSel("select1")
      val caught = intercept[TestFailedException] {
        select1.value = "something else"
      }
      caught.failedCodeLineNumber should be (Some(thisLineNumber - 2))
      caught.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
      caught.getCause.getClass should be (classOf[org.openqa.selenium.NoSuchElementException])
    }
  }

  describe("multiSel") {
    it("should throw TFE with valid stack depth if specified item not found") {
      go to (host + "find-select.html")
      val caught = intercept[TestFailedException] {
        multiSel("unknown")
      }
      caught.failedCodeLineNumber should be (Some(thisLineNumber - 2))
      caught.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
    }
    it("should throw TFE with valid stack depth if specified is found but is not a multiple-selection list") {
      go to (host + "find-select.html")
      val caught = intercept[TestFailedException] {
        multiSel("select1")
      }
      caught.failedCodeLineNumber should be (Some(thisLineNumber - 2))
      caught.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
    }
    it("should, when a valid text field is found, return a MultiSel instance") {
      go to (host + "find-select.html")
      val select2 = multiSel("select2")
      select2.values should be (IndexedSeq("option4", "option5"))
    }
    it("should, when multiple matching multiple-selection lists exist, return the first one") {
      go to (host + "find-select.html")
      val select4 = multiSel("select4")
      select4.values should be (IndexedSeq("option1", "option3"))
    }
    it("should throw TFE with valid stack depth and cause if invalid option is set") {
      go to (host + "find-select.html")
      val select2 = multiSel("select2")
      val caught = intercept[TestFailedException] {
        select2.values = Seq("option6", "something else")
      }
      caught.failedCodeLineNumber should be (Some(thisLineNumber - 2))
      caught.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
      caught.getCause.getClass should be (classOf[org.openqa.selenium.NoSuchElementException])
    }
  }

  describe("click on") {
    it("should throw TFE with valid stack depth if specified item not found", Slow) {
      go to (host + "index.html")
      val caught = intercept[TestFailedException] {
        click on "unknown"
      }
      caught.failedCodeLineNumber should be (Some(thisLineNumber - 2))
      caught.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
    }
    it("should be able to click on element from all query methods") {
      go to (host + "click.html")
      click on id("aLink")
      click on name("aLinkName")
      click on xpath("//html/body/a")
      click on className("aClass")
      click on cssSelector("a[id='aLink']")
      click on linkText("Test Click")
      click on partialLinkText("Click")
      click on tagName("a")
    }
    it("should be able to click on Element") {
      go to (host + "click.html")
      val element = id("aLink").element
      click on element
    }
    it("should be able to click on WebElement") {
      go to (host + "click.html")
      val webElement = id("aLink").webElement
      click on webElement
    }
  }

  describe("switch to") {
    it("should switch frame correctly and throw TFE with valid stack depth if specified frame not found", Slow) {
      go to (host + "frame.html")
      val win = windowHandle
      switch to frame("frame1")
      switch to window(win)
      switch to frame("frame2")
      
      switch to window(win)
      switch to frame(0)
      switch to window(win)
      switch to frame(1)
      
      switch to window(win)
      switch to frame(id("frame1"))
      switch to window(win)
      switch to frame(id("frame2"))
      
      switch to window(win)
      switch to frame(id("frame1").element)
      switch to window(win)
      switch to frame(id("frame2").element)
      
      switch to window(win)
      val caught1= intercept[TestFailedException] {
        switch to frame("frame3")
      }
      caught1.failedCodeLineNumber should be (Some(thisLineNumber - 2))
      caught1.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
      
      val caught2 = intercept[TestFailedException] {
        switch to frame(2)
      }
      caught2.failedCodeLineNumber should be (Some(thisLineNumber - 2))
      caught2.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
      
      val caught3 = intercept[TestFailedException] {
        switch to frame(id("text1"))
      }
      caught3.failedCodeLineNumber should be (Some(thisLineNumber - 2))
      caught3.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
    }
    it("should throw TFE with valid stack depth if specified window handle not found") {
      go to (host + "window.html")
      val handle = windowHandle
      switch to window(handle) // should be ok
      val caught = intercept[TestFailedException] {
        switch to window("Something else")
      }
      caught.failedCodeLineNumber should be (Some(thisLineNumber - 2))
      caught.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
    }
  }

  describe("A MultiSelOptionSeq") {
    it("should allow a string element to be appended with +") {
      val s = new MultiSelOptionSeq(Vector("1", "2", "3"))
      s + "4" should be (new MultiSelOptionSeq(Vector("1", "2", "3", "4")))
    }
    it("should return the same MultiSelOptionSeq if an existing element is passed to +") {
      val s = new MultiSelOptionSeq(Vector("1", "2", "3"))
      s + "2" should be theSameInstanceAs (s)
    }
    it("should allow a string element to be removed with 0") {
      val s = new MultiSelOptionSeq(Vector("1", "2", "3"))
      s - "2" should be (new MultiSelOptionSeq(Vector("1", "3")))
    }
    it("should return the same MultiSelOptionSeq if a non-existing element is passed to 0") {
      val s = new MultiSelOptionSeq(Vector("1", "2", "3"))
      s - "4" should be theSameInstanceAs (s)
    }
  }

  describe("goBack") {
    it("should have no effect if already at oldest page") {
      for (i <- 0 to 1000)
        goBack()
    }
  }

  describe("goForward") {
    it("should have no effect if already at newest page") {
      for (i <- 0 to 1000)
        goForward()
    }
  }

  describe("cookie") {
    it("should throw TFE with valid stack depth if specified cookie is not found") {
      go to (host + "index.html")
      val caught = intercept[TestFailedException] {
        cookie("other")
      }
      caught.failedCodeLineNumber should be (Some(thisLineNumber - 2))
      caught.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
    }
  }

  describe("find") {
    it("should return None if specified item not found") {
      go to (host + "index.html")
      find("something") should be (None)
    }
    it("should return a defined Option[Element] containing an instance of TextField if specified item is found to be a text field") {
      go to (host + "find-textfield.html")
      find("text1") match {
        case Some(textField: TextField) =>
          textField.value should be ("value1")
        case other =>
          fail("Expected Some(textField: TextField), but got: " + other)
      }
    }
    it("should return a defined Option[Element] containing an instance of TextArea if specified item is found to be a text area") {
      go to (host + "find-textarea.html")
      find("textarea1") match {
        case Some(textArea: TextArea) =>
          textArea.text should be ("value1")
        case other => 
          fail("Expected Some(textArea: TextArea), but got: " + other)
      }
    }
    it("should return a defined Option[Element] containing an instance of PasswordField if specified item is found to be a password field") {
      go to (host + "find-pwdfield.html")
      find("secret1") match {
        case Some(pwdField: PasswordField) =>
          pwdField.value should be ("pwd1")
        case other =>
          fail("Expected Some(pwdField: PasswordField), but got: " + other)
      }
    }
    it("should return a defined Option[Element] containing an instance of RadioButton if specified item is found to be a radio button") {
      go to (host + "find-radio.html")
      find("group2") match {
        case Some(radio: RadioButton) =>
          radio.value should be ("value2")
        case other => 
          fail("Expected Some(radio: RadioButton), but got: " + other)
      }
    }
    it("should return a defined Option[Element] containing an instance of Checkbox if specified item is found to be a checkbox") {
      go to (host + "find-checkbox.html")
      find("opt1") match {
        case Some(checkbox: Checkbox) => 
          checkbox.isSelected should be (true)
        case other => 
          fail("Expected Some(checkbox: Checkbox), but got: " + other)
      }
    }
    it("should return a defined Option[Element] containing an instance of SingleSel if specified item is found to be a single-selection list") {
      go to (host + "find-select.html")
      find("select1") match {
        case Some(singleSel: SingleSel) => 
          singleSel.value should be ("option2")
        case other => 
          fail("Expected Some(singleSel: SingleSel), but got: " + other)
      }
    }
    it("should return a defined Option[Element] containing an instance of MultiSel if specified item is found to be a multiple-selection list") {
      go to (host + "find-select.html")
      find("select2") match {
        case Some(multiSel: MultiSel) => 
          multiSel.values should be (IndexedSeq("option4", "option5"))
        case other =>
          fail("Expected Some(multiSel: MultiSel), but got: " + other)
      }
    }
    it("should return a defined Option[Element] containing an instance of Element if specified item is found but is not one of the items for which we have defined an Element subclass") {
      go to (host + "image.html")
      find("anImage") match {
        case Some(element: Element) => 
          element.tagName should be ("img")
        case other => 
          fail("Expected Some(element: Element), but got: " + other)
      }
    }
  }

  describe("findAll") {
    it("should return an empty Iterator if specified item not found") {
      go to (host + "index.html")
      findAll("something").hasNext should be (false)
    }
    it("should return a defined Iterator[Element] containing an instance of TextField if specified item is found to be a text field") {
      go to (host + "find-textfield.html")
      val text1 = findAll("text1")
      text1.hasNext should be (true)
      text1.next match {
        case textField: TextField => 
          textField.value should be ("value1")
        case other => 
          fail("Expected TextField, but got: " + other)
      }
    }
    it("should return a defined Iterator[Element] containing an instance of TextArea if specified item is found to be a text area") {
      go to (host + "find-textarea.html")
      val textarea1 = findAll("textarea1")
      textarea1.hasNext should be (true)
      textarea1.next match {
        case textArea: TextArea =>
          textArea.text should be ("value1")
        case other =>
          fail("Expected TextArea, but got: " + other)
      }
    }
    it("should return a defined Iterator[Element] containing an instance of PasswordField if specified item is found to be a password field") {
      go to (host + "find-pwdfield.html")
      val secret1 = findAll("secret1")
      secret1.hasNext should be (true)
      secret1.next match {
        case pwdField: PasswordField => 
          pwdField.value should be ("pwd1")
        case other => 
          fail("Expected PasswordField, but got: " + other)
      }
    }
    it("should return a defined Iterator[Element] containing an instance of RadioButton if specified item is found to be a radio button") {
      go to (host + "find-radio.html")
      val group1 = findAll("group1")
      group1.hasNext should be (true)
      group1.next match {
        case radio: RadioButton =>
          radio.value should be ("value1")
        case other =>
          fail("Expected RadioButton, but got: " + other)
      }
    }
    it("should return a defined Iterator[Element] containing an instance of Checkbox if specified item is found to be a checkbox") {
      go to (host + "find-checkbox.html")
      val opt1 = findAll("opt1")
      opt1.hasNext should be (true)
      opt1.next match {
        case checkbox: Checkbox =>
          checkbox.value should be ("Option 1")
        case other =>
          fail("Expected Checkbox, but got: " + other)
      }
    }
    it("should return a defined Iterator[Element] containing an instance of SingleSel if specified item is found to be a single-selection list") {
      go to (host + "find-select.html")
      val select1 = findAll("select1")
      select1.hasNext should be (true)
      select1.next match {
        case singleSel: SingleSel => 
          singleSel.value should be ("option2")
        case other =>
          fail("Expected SingleSel, but got: " + other)
      }
    }
    it("should return a defined Iterator[Element] containing an instance of MultiSel if specified item is found to be a multiple-selection list") {
      go to (host + "find-select.html")
      val select2 = findAll("select2")
      select2.hasNext should be (true)
      select2.next match {
        case multiSel: MultiSel => 
          multiSel.values should be (IndexedSeq("option4", "option5"))
        case other =>
          fail("Expected MultiSel, but got: " + other)
      }
    }
    it("should return a defined Iterator[Element] containing an instance of Element if specified item is found but is not one of the items for which we have defined an Element subclass") {
      go to (host + "image.html")
      val anImage = findAll("anImage")
      anImage.hasNext should be (true)
      anImage.next match {
        case image: Element =>
          image.tagName should be ("img")
        case other =>
          fail("Expected Element, but got: " + other)  
      }
    }
  }

  describe("executeScript") {
    it("should execute the passed JavaScript") {
      go to (host + "index.html")
      val result1 = executeScript("return document.title;")
      result1 should be ("Test Title")
      val result2 = executeScript("return 'Hello ' + arguments[0]", "ScalaTest")
      result2 should be ("Hello ScalaTest")
    }
  }
  
  describe("executeAsyncScript") {
    it("should execute the passed JavaScript with asynchronous call") {
      go to (host + "index.html")
      val script = """
        var callback = arguments[arguments.length - 1];
        window.setTimeout(function() {callback('Hello ScalaTest')}, 500);
        """
      setScriptTimeout(1 second)
      val result = executeAsyncScript(script)
      result should be ("Hello ScalaTest")
    }
  }

  describe("Web Browser") {

    it("should go to web page by using url and get its title correctly.") {
      go to (host + "index.html")
      pageTitle should be ("Test Title")
    }
    
    it("should go to web page by using Page and get its title correctly.") {
      class RadioPage extends Page {
        val url = host + "radio.html"
      }
      val radioPage = new RadioPage
      go to radioPage
      pageTitle should be ("Radio Button")
    }

    it("should get and set text field value correctly.", Slow) {
      go to (host + "textfield.html")
      pageTitle should be ("Text Field")

      textField("text1").value should be ("")                   
                                                                // textField("text1") should have ('value(""), 'attribute(""))
      textField("text1").attribute("value") should be (Some(""))           // textField("text1").attribute("value") should be ("")    // ok as is
      textField("text1").value = "value 1"                          // set textField "text1" to "value 1"
      textField("text1").value should be ("value 1")              // textField("text1").text should be ("value 1")
      textField("text1").attribute("value") should be (Some("value 1"))    // textField("text1").attribute("value") should be ("value 1")

      textField("text2").value should be ("")
      textField("text2").attribute("value") should be (Some(""))
      textField("text2").value = "value 2"
      textField("text2").value should be ("value 2")
      textField("text2").attribute("value") should be (Some("value 2"))
    }
    
    it("should get and set password field value correctly.", Slow) {
      go to (host + "pwdfield.html")
      pageTitle should be ("Password Field")

      pwdField("secret1").value should be ("")                   

      pwdField("secret1").attribute("value") should be (Some(""))
      pwdField("secret1").value = "value 1"
      pwdField("secret1").value should be ("value 1")
      pwdField("secret1").attribute("value") should be (Some("value 1"))

      pwdField("secret2").value should be ("")
      pwdField("secret2").attribute("value") should be (Some(""))
      pwdField("secret2").value = "value 2"
      pwdField("secret2").value should be ("value 2")
      pwdField("secret2").attribute("value") should be (Some("value 2"))
    }

    it("should allow text to be entered in the active element if it is a text field.") {

      go to (host + "textfield.html")
      pageTitle should be ("Text Field")
                                                                
      textField("text1").value should be ("")                   

      click on "text1"
      enter("value 1A")
      textField("text1").value should be ("value 1A")                   
      enter("value 1B")
      textField("text1").value should be ("value 1B")                   
      enter("")
      textField("text1").value should be ("")                   

      pressKeys("first post!")
      textField("text1").value should be ("first post!")                   
      pressKeys(" second post!")
      textField("text1").value should be ("first post! second post!")                   
      pressKeys(" third post!")
      textField("text1").value should be ("first post! second post! third post!")                   
    }
    
    it("should allow text to be entered in the active element if it is a password field.") {

      go to (host + "pwdfield.html")
      pageTitle should be ("Password Field")
                                                                
      pwdField("secret1").value should be ("")                   

      click on "secret1"
      enter("secret 1A")
      pwdField("secret1").value should be ("secret 1A")                   
      enter("secret 1B")
      pwdField("secret1").value should be ("secret 1B")                   
      enter("")
      pwdField("secret1").value should be ("")                   

      pressKeys("first secret!")
      pwdField("secret1").value should be ("first secret!")                   
      pressKeys(" second secret!")
      pwdField("secret1").value should be ("first secret! second secret!")                   
      pressKeys(" third secret!")
      pwdField("secret1").value should be ("first secret! second secret! third secret!")                   
    }
    
    it("should throw TestFailedException with correct stack depth when enter is called currently selected element is neither text field, text area or password field.") {
      go to (host + "checkbox.html")
      click on ("opt1")
      val e = intercept[TestFailedException] {
        enter("Some text")
      }
      e.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
    }

    it("should clear a text field.") {
      go to (host + "textfield.html")
      pageTitle should be ("Text Field")
                                                                
      textField("text1").value = "value 1"                     
      textField("text1").value should be ("value 1")

      textField("text1").clear()
      textField("text1").value should be ("")                   

      textField("text1").value = "value 1"                     
      textField("text1").value should be ("value 1")
    }
 
    it("should get and set text area value correctly.", Slow) {
      go to (host + "textarea.html")
      pageTitle should be ("Text Area")
      
      textArea("area1").value should be ("")
      textArea("area1").attribute("value") should be (Some(""))
      textArea("area1").value = "area 1 - line 1\narea 1 - line 2"
      textArea("area1").value should be ("area 1 - line 1\narea 1 - line 2")
      textArea("area1").attribute("value") should be (Some("area 1 - line 1\narea 1 - line 2"))
      
      textArea("area2").value should be ("")
      textArea("area2").attribute("value") should be (Some(""))
      textArea("area2").value = "area 2 - line 1\narea 2 - line 2"
      textArea("area2").value should be ("area 2 - line 1\narea 2 - line 2")
      textArea("area2").attribute("value") should be (Some("area 2 - line 1\narea 2 - line 2"))
    }
    
    it("should clear a text area.") {
      go to (host + "textarea.html")
      pageTitle should be ("Text Area")
      
      textArea("area1").value = "area 1 - line 1\narea 1 - line 2"
      textArea("area1").value should be ("area 1 - line 1\narea 1 - line 2")

      textArea("area1").clear()
      textArea("area1").value should be ("")
    }
    
    it("should clear a password field.") {
      go to (host + "pwdfield.html")
      pageTitle should be ("Password Field")
                                                                
      pwdField("secret1").value = "value 1"                     
      pwdField("secret1").value should be ("value 1")

      pwdField("secret1").clear()
      pwdField("secret1").value should be ("")                   

      pwdField("secret1").value = "value 1"                     
      pwdField("secret1").value should be ("value 1")
    }
    
    it("should allow text to be entered in the active element if it is a text area.") {
      go to (host + "textarea.html")
      pageTitle should be ("Text Area")
      
      textArea("area1").value should be ("")

      click on "area1"
      enter("area 1 - line 1\narea 1 - line 2")
      textArea("area1").value should be ("area 1 - line 1\narea 1 - line 2")                   
      enter("area 1 - line 0\narea 1 - line 1\narea 1 - line 2")
      textArea("area1").value should be ("area 1 - line 0\narea 1 - line 1\narea 1 - line 2")                   
      enter("")
      textArea("area1").value should be ("")                   

      pressKeys("line 1\n")
      textArea("area1").value should be ("line 1\n")                   
      pressKeys("line 2")
      textArea("area1").value should be ("line 1\nline 2")                   
      pressKeys("b or not 2b")
      textArea("area1").value should be ("line 1\nline 2b or not 2b")                   
    }
    
    it("should get and set radio button group correctly.") {
      go to (host + "radio.html")
      pageTitle should be ("Radio Button")
      
      radioButtonGroup("group1").selection should be (None)
      intercept[TestFailedException] {
        radioButtonGroup("group1").value
      }
      
      radioButtonGroup("group1").value = "Option 1"
      radioButtonGroup("group1").value should be ("Option 1")
      
      radioButtonGroup("group1").value = "Option 2"
      radioButtonGroup("group1").value should be ("Option 2")
      
      radioButtonGroup("group1").value = "Option 3"
      radioButtonGroup("group1").value should be ("Option 3")
    }
    
    it("should fail with TestFailedException with correct stack depth and cause when invalid value is set") {
      go to (host + "radio.html")
      val e = intercept[org.scalatest.exceptions.TestFailedException] {
        radioButtonGroup("group1").value = "Invalid value"
      }
      e.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
    }
    
    it("should read, select and clear check box correctly.") {
      go to (host + "checkbox.html")
      pageTitle should be ("Check Box")
      
      checkbox("opt1").isSelected should be (false)
      checkbox("opt1").select()
      checkbox("opt1").isSelected should be (true)
      checkbox("opt1").clear()
      checkbox("opt1").isSelected should be (false)
      
      checkbox("opt2").isSelected should be (false)
      checkbox("opt2").select()
      checkbox("opt2").isSelected should be (true)
      checkbox("opt2").clear()
      checkbox("opt1").isSelected should be (false)
    }

    it("should read, select and clear dropdown list (select) correctly.", Slow) {
      go to (host + "select.html")
      pageTitle should be ("Select")
      
      singleSel("select1").value should be ("option1")
      singleSel("select1").value = "option2"
      singleSel("select1").value should be ("option2")
      singleSel("select1").value = "option3"
      singleSel("select1").value should be ("option3")
      singleSel("select1").value = "option1"
      singleSel("select1").value should be ("option1")
      intercept[TestFailedException] {
        singleSel("select1").value = "other"
      }
      
      // No options selected
      // multiSel("select2").selections should be (None)
      multiSel("select2").values should have size 0
      multiSel("select2").values += "option4"
      multiSel("select2").values should be (IndexedSeq("option4"))
      multiSel("select2").values += "option5"
      multiSel("select2").values should be (IndexedSeq("option4", "option5"))
      multiSel("select2").values should have size 2
      multiSel("select2").values(0) should be ("option4")
      multiSel("select2").values(1) should be ("option5")
      multiSel("select2").values += "option6"
      multiSel("select2").values should be (IndexedSeq("option4", "option5", "option6"))
      multiSel("select2").values should have size 3
      multiSel("select2").values(0) should be ("option4")
      multiSel("select2").values(1) should be ("option5")
      multiSel("select2").values(2) should be ("option6")
      // multiSel("select2").selections should be (Some(IndexedSeq("option4", "option5", "option6")))
      intercept[TestFailedException] {
        multiSel("select2").values += "other"
      }
      multiSel("select2").values -= "option5"
      multiSel("select2").values should have size 2
      multiSel("select2").values(0) should be ("option4")
      multiSel("select2").values(1) should be ("option6")
      multiSel("select2").clearAll()
      // multiSel("select2").selections should be (None)
      multiSel("select2").values should have size 0
      
      // Test the alternative way to clear
      multiSel("select2").values += "option6"
      multiSel("select2").values should have size 1
      multiSel("select2").values(0) should be ("option6")
      multiSel("select2") clear "option6"
      // multiSel("select2").selections should be (None)
      multiSel("select2").values should have size 0
    }
    
    it("should submit form when submit is called on form's element.") {
      go to (host + "submit.html")
      pageTitle should be ("Submit")
      
      click on "name" // This set the focus
      textField("name").value = "Penguin"
      submit()
      // submit (name("name")) // This will work as well.
    }
    
    it("should throw TestFailedException with correct stack depth when submit is called on none form's element") {
      go to (host + "submit.html")
      pageTitle should be ("Submit")
      
      val e = intercept[TestFailedException] {
        click on "not_form_element" // This set the focus
        textField("not_form_element").value = "Penguin"
        submit()
      }
      e.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
    }
    
    it("should submit form when submit button is clicked.") {
      go to (host + "submit.html")
      pageTitle should be ("Submit")
      
      textField("name").value = "Penguin"
      click on "submitButton"
    }
    
    it("should navigate to, back, forward and refresh correctly") {
      go to (host + "navigate1.html")
      pageTitle should be ("Navigation 1")
      go to (host + "navigate2.html")
      pageTitle should be ("Navigation 2")
      goBack()
      // click back button
      pageTitle should be ("Navigation 1")
      goForward()
      // click forward button
      pageTitle should be ("Navigation 2")
      reloadPage()
      // click refreshPage button
      // click submit button
      pageTitle should be ("Navigation 2")
    }
    
    it("should support goBack, goForward and reloadPage correctly") {
      go to (host + "navigate1.html")
      pageTitle should be ("Navigation 1")
      go to (host + "navigate2.html")
      pageTitle should be ("Navigation 2")
      goBack()
      pageTitle should be ("Navigation 1")
      goForward()
      pageTitle should be ("Navigation 2")
      reloadPage()
      pageTitle should be ("Navigation 2")
    }
    
    it("should create, read and delete cookie correctly") {
      go to (host + "index.html")
      
      add cookie("name1", "value1")
      cookie("name1").value should be ("value1")
      
      add cookie("name2", "value2")
      cookie("name2").value should be ("value2")
      
      add cookie("name3", "value3")
      cookie("name3").value should be ("value3")
      
      delete cookie "name2"
      val e1 = intercept[TestFailedException] {
        cookie("name2") should be (null)
      }
      e1.failedCodeLineNumber should be (Some(thisLineNumber - 2))
      e1.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
      cookie("name1").value should be ("value1")
      cookie("name3").value should be ("value3")
      
      delete all cookies
      val e2 = intercept[TestFailedException] {
        cookie("name1") should be (null)
      }
      e2.failedCodeLineNumber should be (Some(thisLineNumber - 2))
      e2.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
      
      val e3 = intercept[TestFailedException] {
        cookie("name3") should be (null)
      }
      e3.failedCodeLineNumber should be (Some(thisLineNumber - 2))
      e3.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
      
      val e4 = intercept[TestFailedException] {
        delete cookie ("name1")
      }
      e4.failedCodeLineNumber should be (Some(thisLineNumber - 2))
      e4.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
    }
    
    it("should create, read and delete cookie correctly using libraryish alternatives") {
      goTo(host + "index.html")
      
      addCookie("name1", "value1")
      cookie("name1").value should be ("value1")
      
      addCookie("name2", "value2")
      cookie("name2").value should be ("value2")
      
      addCookie("name3", "value3")
      cookie("name3").value should be ("value3")
      
      deleteCookie("name2")
      val e1 = intercept[TestFailedException] {
        cookie("name2") should be (null)  // TODO: This should throw a TFE not return null
      }
      e1.failedCodeLineNumber should be (Some(thisLineNumber - 2))
      e1.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
      cookie("name1").value should be ("value1")
      cookie("name3").value should be ("value3")
      
      deleteAllCookies()
      val e2 = intercept[TestFailedException] {
        cookie("name1") should be (null)
      }
      e2.failedCodeLineNumber should be (Some(thisLineNumber - 2))
      e2.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
      
      val e3 = intercept[TestFailedException] {
        cookie("name3") should be (null)
      }
      e3.failedCodeLineNumber should be (Some(thisLineNumber - 2))
      e3.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
      
      val e4 = intercept[TestFailedException] {
        deleteCookie("name1")
      }
      e4.failedCodeLineNumber should be (Some(thisLineNumber - 2))
      e4.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
    }
    
    it("should support implicitlyWait method") {
      implicitlyWait(Span(2, Seconds))
    }
  
    it("should support capturing screenshot", Slow) {
      go to "http://www.artima.com"
      try {
        capture
        capture to ("MyScreenShot.png")
        captureTo("MyScreenShot2.png")
      }
      catch {
        case unsupported: UnsupportedOperationException => 
          cancel(unsupported)
      }
    }
    
    it("should support setting capture target directory", Slow) {
      go to "http://www.artima.com"
      val tempDir = new File(System.getProperty("java.io.tmpdir"))
      val targetDir1 = new File(tempDir, "scalatest-test-target1")
      val targetDir2 = new File(tempDir, "scalatest-test-target2")
      
      try {
        setCaptureDir(targetDir1.getAbsolutePath)
        capture to ("MyScreenShot.png")
        new File(targetDir1, "MyScreenShot.png").exists should be (true)
        
        setCaptureDir(targetDir2.getAbsolutePath)
        captureTo("MyScreenShot2.png")
        new File(targetDir2, "MyScreenShot2.png").exists should be (true)
      }
      catch {
        case unsupported: UnsupportedOperationException => 
          cancel(unsupported)
      }
    }
    
    it("isScreenshotSupported should return false for HtmlUnitDriver") {
      val driver = try new HtmlUnitDriver catch { case e: Throwable => cancel(e) }
      try isScreenshotSupported(driver) should be (false)
      finally close()(driver)
    }

    /* This is causing long hang times and left-open Firefox windows.
     * TODO: fix
     */
    ignore("isScreenshotSupported should return true for FirefoxDriver") {
      val driver = try {
        new FirefoxDriver(new FirefoxProfile())
      }
      catch { case e: Throwable => cancel(e) }
      try isScreenshotSupported(driver) should be (true)
      finally close()(driver)
    }
  
    /* Safari is blowing up on Bill's Mac leaving windows open and hanging for 
       a long time before finally giving up. Get:
       [scalatest] Aug 3, 2012 4:39:18 AM org.openqa.selenium.safari.SafariDriverServer start
       [scalatest] INFO: Server started at http://Mi-Novia.local:38801/
       [scalatest] - isScreenshotSupported should return false for SafariDriver !!! CANCELED !!!
       [scalatest]   Could not start a new session. Possible causes are invalid address of the remote server or browser start-up failure.
       [scalatest] Build info: version: 'unknown', revision: 'unknown', time: 'unknown'
       [scalatest] System info: os.name: 'Mac OS X', os.arch: 'x86_64', os.version: '10.7.4', java.version: '1.6.0_33'
       [scalatest] Driver info: driver.version: SafariDriver (WebBrowserSpec.scala:732)
    */
    ignore("isScreenshotSupported should return true for SafariDriver") {
      val driver = try new SafariDriver catch { case e: Throwable => cancel(e) }
      try isScreenshotSupported(driver) should be (true)
      finally close()(driver)
    }
  
    it("isScreenshotSupported should return true for ChromeDriver") {
      val driver = try new ChromeDriver catch { case e: Throwable => cancel(e) }
      try isScreenshotSupported(driver) should be (true)
      finally close()(driver)
    }
  
    it("isScreenshotSupported should return true for InternetExplorerDriver") {
      val driver = try new InternetExplorerDriver catch { case e: Throwable => cancel(e) }
      try isScreenshotSupported(driver) should be (true)
      finally close()(driver)
    }
  
/*
    ignore("should support wait method") {
      // This example is taken from http://seleniumhq.org/docs/03_webdriver.html
  
      // Visit Google
      go to "http://www.google.com"
      // Alternatively the same thing can be done like this
      // navigate to "http://www.google.com"

      // Click on the text input element by its name 
      click on "q"
      // and enter "Cheese!"
      textField("q").value = "Cheese!"

      // Now submit the form
      submit()
        
      // Google's search is rendered dynamically with JavaScript.
      // Wait for the page to load, timeout after 10 seconds
      wait[Boolean](Span(10, Seconds)) {
        pageTitle.toLowerCase.startsWith("cheese!")
      }

      // Should see: "cheese! - Google Search"
      pageTitle should be ("Cheese! - Google Search")
    }
*/
 
    ignore("should be able to use ScalaTest's eventually in place of Selenium's wait") {
      import org.scalatest.concurrent.Eventually._

      go to "http://www.google.com"
      click on "q"
      textField("q").value = "Cheese!"
      submit()
      
      eventually(timeout(10 seconds)) {
        pageTitle.toLowerCase.startsWith("cheese!") should be (true)
      }
      
      pageTitle should be ("Cheese! - Google Search")
    }
        
    // Some operation not supported in HtmlUnit driver, e.g. switch to alert.
    // Should be good enough to test the following dsl compiles.
    ignore("should support switch to") {
      switch to activeElement
      switch to alert
      switch to defaultContent
      switch to frame(0)
      switch to frame("name")
      switch to frame(name("name"))
      switch to frame(name("name").element)
      switch to window(windowHandle)
    }
    
    ignore("should support switchTo") {
      switchTo(activeElement)
      switchTo(alert)
      switchTo(defaultContent)
      switchTo(frame(0))
      switchTo(frame("name"))
      switchTo(frame(name("name")))
      switchTo(frame(name("name").element))
      switchTo(window(windowHandle))
    }
  }
  
  describe("WrappedCookie") {
    it("should wrap null expiry in an option") {
      val cookie = new WrappedCookie(new Cookie("name", "value", "path", null))
      cookie.expiry should be (None)
    }
  }
  
  describe("goTo") {
    it("should go to given url correctly") {
      goTo(host + "index.html")
      pageTitle should be ("Test Title")
      goTo(host + "textfield.html")
      pageTitle should be ("Text Field")
    }
    it("should go to given page correctly") {
      class IndexPage extends Page {
        val url = host + "index.html"
      }
      class TextFieldPage extends Page {
        val url = host + "textfield.html"
      }
      val indexPage = new IndexPage
      val textFieldPage = new TextFieldPage
      goTo(indexPage)
      pageTitle should be ("Test Title")
      goTo(textFieldPage)
      pageTitle should be ("Text Field")
    }
  }
  
  describe("clickOn") {
    it("should throw TFE with valid stack depth if specified item not found", Slow) {
      go to (host + "index.html")
      val caught = intercept[TestFailedException] {
        clickOn("unknown")
      }
      caught.failedCodeLineNumber should be (Some(thisLineNumber - 2))
      caught.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
    }
    it("should be able to clickOn element from all query methods") {
      go to (host + "click.html")
      clickOn(id("aLink"))
      clickOn(name("aLinkName"))
      clickOn(xpath("//html/body/a"))
      clickOn(className("aClass"))
      clickOn(cssSelector("a[id='aLink']"))
      clickOn(linkText("Test Click"))
      clickOn(partialLinkText("Click"))
      clickOn(tagName("a"))
    }
    it("should be able to clickOn Element") {
      go to (host + "click.html")
      val element = id("aLink").element
      clickOn(element)
    }
    it("should be able to click on WebElement") {
      go to (host + "click.html")
      val webElement = id("aLink").webElement
      clickOn(webElement)
    }
  }
  
  describe("Query") {
    describe("id") {
      it("should return Element correctly when element method is called with valid id") {
        go to (host + "click.html")
        id("aLink").element.isInstanceOf[Element] should be (true)
      }
      it("should throw TestFailedException with correct stack depth and cause when element method is called with invalid id", Slow) {
        go to (host + "click.html")
        val e = intercept[TestFailedException] {
          id("aInvalidLink").element
        }
        e.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        e.getCause.isInstanceOf[org.openqa.selenium.NoSuchElementException] should be (true)
      }
      it("should return Some(Element) when findElement method is called with valid id") {
        go to (host + "click.html")
        id("aLink").findElement match {
          case Some(element: Element) => // ok
          case other => fail("Expected Some(element: Element), but got: " + other)
        }
      }
      it("should return None when findElement method is called with invalid id", Slow) {
        go to (host + "click.html")
        id("aInvalidLink").findElement should be (None)
      }
      it("should return WebElement correctly when webElement method is called with valid id") {
        go to (host + "click.html")
        id("aLink").webElement.isInstanceOf[WebElement] should be (true)
      }
      it("should throw TestFailedException with correct stack depth and cause when webElement method is called with invalid id", Slow) {
        go to (host + "click.html")
        val e = intercept[TestFailedException] {
          id("aInvalidLink").webElement
        }
        e.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        e.getCause.isInstanceOf[org.openqa.selenium.NoSuchElementException] should be (true)
      }
      it("should return list Iterator[Element] with size of 1 when findAllElements is called with valid id") {
        go to (host + "click.html")
        val itr = id("aLink").findAllElements
        itr.hasNext should be (true)
        itr.next.isInstanceOf[Element] should be (true)
        itr.hasNext should be (false)
      }
      it("should return list Iterator[Element] with size of 0 when findAllElements is called with invalid id", Slow) {
        go to (host + "click.html")
        val itr = id("aInvalidLink").findAllElements
        itr.hasNext should be (false)
      }
    }
    
    describe("name") {
      it("should return Element correctly when element method is called with valid name") {
        go to (host + "click.html")
        name("aLinkName").element.isInstanceOf[Element] should be (true)
      }
      it("should throw TestFailedException with correct stack depth and cause when element method is called with invalid name", Slow) {
        go to (host + "click.html")
        val e = intercept[TestFailedException] {
          name("aInvalidLinkName").element
        }
        e.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        e.getCause.isInstanceOf[org.openqa.selenium.NoSuchElementException] should be (true)
      }
      it("should return Some(Element) when findElement method is called with valid name") {
        go to (host + "click.html")
        name("aLinkName").findElement match {
          case Some(element: Element) => // ok
          case other => fail("Expected Some(element: Element), but got: " + other)
        }
      }
      it("should return None when findElement method is called with invalid name", Slow) {
        go to (host + "click.html")
        name("aInvalidLinkName").findElement should be (None)
      }
      it("should return WebElement correctly when webElement method is called with valid name") {
        go to (host + "click.html")
        name("aLinkName").webElement.isInstanceOf[WebElement] should be (true)
      }
      it("should throw TestFailedException with correct stack depth and cause when webElement method is called with invalid name", Slow) {
        go to (host + "click.html")
        val e = intercept[TestFailedException] {
          name("aInvalidLinkName").webElement
        }
        e.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        e.getCause.isInstanceOf[org.openqa.selenium.NoSuchElementException] should be (true)
      }
      it("should return list Iterator[Element] with size of 1 when findAllElements is called with valid name") {
        go to (host + "click.html")
        val itr = name("aLinkName").findAllElements
        itr.hasNext should be (true)
        itr.next.isInstanceOf[Element] should be (true)
        itr.hasNext should be (false)
      }
      it("should return list Iterator[Element] with size of 0 when findAllElements is called with invalid name", Slow) {
        go to (host + "click.html")
        val itr = name("aInvalidLinkName").findAllElements
        itr.hasNext should be (false)
      }
    }
    
    describe("xpath") {
      it("should return Element correctly when element method is called with valid xpath") {
        go to (host + "click.html")
        xpath("//html/body/a").element.isInstanceOf[Element] should be (true)
      }
      it("should throw TestFailedException with correct stack depth and cause when element method is called with invalid xpath", Slow) {
        go to (host + "click.html")
        val e = intercept[TestFailedException] {
          xpath("//html/body/div/a").element
        }
        e.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        e.getCause.isInstanceOf[org.openqa.selenium.NoSuchElementException] should be (true)
      }
      it("should return Some(Element) when findElement method is called with valid xpath") {
        go to (host + "click.html")
        xpath("//html/body/a").findElement match {
          case Some(element: Element) => // ok
          case other => fail("Expected Some(element: Element), but got: " + other)
        }
      }
      it("should return None when findElement method is called with invalid xpath", Slow) {
        go to (host + "click.html")
        xpath("//html/body/div/a").findElement should be (None)
      }
      it("should return WebElement correctly when webElement method is called with valid xpath") {
        go to (host + "click.html")
        xpath("//html/body/a").webElement.isInstanceOf[WebElement] should be (true)
      }
      it("should throw TestFailedException with correct stack depth and cause when webElement method is called with invalid xpath", Slow) {
        go to (host + "click.html")
        val e = intercept[TestFailedException] {
          xpath("//html/body/div/a").webElement
        }
        e.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        e.getCause.isInstanceOf[org.openqa.selenium.NoSuchElementException] should be (true)
      }
      it("should return list Iterator[Element] with size of 1 when findAllElements is called with valid xpath") {
        go to (host + "click.html")
        val itr = xpath("//html/body/a").findAllElements
        itr.hasNext should be (true)
        itr.next.isInstanceOf[Element] should be (true)
        itr.hasNext should be (false)
      }
      it("should return list Iterator[Element] with size of 0 when findAllElements is called with invalid xpath", Slow) {
        go to (host + "click.html")
        val itr = xpath("//html/body/div/a").findAllElements
        itr.hasNext should be (false)
      }
    }
    
    describe("className") {
      it("should return Element correctly when element method is called with valid className") {
        go to (host + "click.html")
        className("aClass").element.isInstanceOf[Element] should be (true)
      }
      it("should throw TestFailedException with correct stack depth and cause when element method is called with invalid className", Slow) {
        go to (host + "click.html")
        val e = intercept[TestFailedException] {
          className("aInvalidClass").element
        }
        e.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        e.getCause.isInstanceOf[org.openqa.selenium.NoSuchElementException] should be (true)
      }
      it("should return Some(Element) when findElement method is called with valid className") {
        go to (host + "click.html")
        className("aClass").findElement match {
          case Some(element: Element) => // ok
          case other => fail("Expected Some(element: Element), but got: " + other)
        }
      }
      it("should return None when findElement method is called with invalid className", Slow) {
        go to (host + "click.html")
        className("aInvalidClass").findElement should be (None)
      }
      it("should return WebElement correctly when webElement method is called with valid className") {
        go to (host + "click.html")
        className("aClass").webElement.isInstanceOf[WebElement] should be (true)
      }
      it("should throw TestFailedException with correct stack depth and cause when webElement method is called with invalid className", Slow) {
        go to (host + "click.html")
        val e = intercept[TestFailedException] {
          className("aInvalidClass").webElement
        }
        e.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        e.getCause.isInstanceOf[org.openqa.selenium.NoSuchElementException] should be (true)
      }
      it("should return list Iterator[Element] with size of 1 when findAllElements is called with valid className") {
        go to (host + "click.html")
        val itr = className("aClass").findAllElements
        itr.hasNext should be (true)
        itr.next.isInstanceOf[Element] should be (true)
        itr.hasNext should be (false)
      }
      it("should return list Iterator[Element] with size of 0 when findAllElements is called with invalid className", Slow) {
        go to (host + "click.html")
        val itr = className("aInvalidClass").findAllElements
        itr.hasNext should be (false)
      }
    }
    
    describe("cssSelector") {
      it("should return Element correctly when element method is called with valid cssSelector") {
        go to (host + "click.html")
        cssSelector("a[id='aLink']").element.isInstanceOf[Element] should be (true)
      }
      it("should throw TestFailedException with correct stack depth and cause when element method is called with invalid cssSelector", Slow) {
        go to (host + "click.html")
        val e = intercept[TestFailedException] {
          cssSelector("a[id='aInvalidLink']").element
        }
        e.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        e.getCause.isInstanceOf[org.openqa.selenium.NoSuchElementException] should be (true)
      }
      it("should return Some(Element) when findElement method is called with valid cssSelector") {
        go to (host + "click.html")
        cssSelector("a[id='aLink']").findElement match {
          case Some(element: Element) => // ok
          case other => fail("Expected Some(element: Element), but got: " + other)
        }
      }
      it("should return None when findElement method is called with invalid cssSelector", Slow) {
        go to (host + "click.html")
        cssSelector("a[id='aInvalidLink']").findElement should be (None)
      }
      it("should return WebElement correctly when webElement method is called with valid cssSelector") {
        go to (host + "click.html")
        cssSelector("a[id='aLink']").webElement.isInstanceOf[WebElement] should be (true)
      }
      it("should throw TestFailedException with correct stack depth and cause when webElement method is called with invalid cssSelector", Slow) {
        go to (host + "click.html")
        val e = intercept[TestFailedException] {
          cssSelector("a[id='aInvalidLink']").webElement
        }
        e.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        e.getCause.isInstanceOf[org.openqa.selenium.NoSuchElementException] should be (true)
      }
      it("should return list Iterator[Element] with size of 1 when findAllElements is called with valid cssSelector") {
        go to (host + "click.html")
        val itr = cssSelector("a[id='aLink']").findAllElements
        itr.hasNext should be (true)
        itr.next.isInstanceOf[Element] should be (true)
        itr.hasNext should be (false)
      }
      it("should return list Iterator[Element] with size of 0 when findAllElements is called with invalid cssSelector", Slow) {
        go to (host + "click.html")
        val itr = cssSelector("a[id='aInvalidLink']").findAllElements
        itr.hasNext should be (false)
      }
    }
    
    describe("linkText") {
      it("should return Element correctly when element method is called with valid linkText") {
        go to (host + "click.html")
        linkText("Test Click").element.isInstanceOf[Element] should be (true)
      }
      it("should throw TestFailedException with correct stack depth and cause when element method is called with invalid linkText", Slow) {
        go to (host + "click.html")
        val e = intercept[TestFailedException] {
          linkText("Test Invalid Click").element
        }
        e.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        e.getCause.isInstanceOf[org.openqa.selenium.NoSuchElementException] should be (true)
      }
      it("should return Some(Element) when findElement method is called with valid linkText") {
        go to (host + "click.html")
        linkText("Test Click").findElement match {
          case Some(element: Element) => // ok
          case other => fail("Expected Some(element: Element), but got: " + other)
        }
      }
      it("should return None when findElement method is called with invalid linkText", Slow) {
        go to (host + "click.html")
        linkText("Test Invalid Click").findElement should be (None)
      }
      it("should return WebElement correctly when webElement method is called with valid linkText") {
        go to (host + "click.html")
        linkText("Test Click").webElement.isInstanceOf[WebElement] should be (true)
      }
      it("should throw TestFailedException with correct stack depth and cause when webElement method is called with invalid linkText", Slow) {
        go to (host + "click.html")
        val e = intercept[TestFailedException] {
          linkText("Test Invalid Click").webElement
        }
        e.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        e.getCause.isInstanceOf[org.openqa.selenium.NoSuchElementException] should be (true)
      }
      it("should return list Iterator[Element] with size of 1 when findAllElements is called with valid linkText") {
        go to (host + "click.html")
        val itr = linkText("Test Click").findAllElements
        itr.hasNext should be (true)
        itr.next.isInstanceOf[Element] should be (true)
        itr.hasNext should be (false)
      }
      it("should return list Iterator[Element] with size of 0 when findAllElements is called with invalid linkText", Slow) {
        go to (host + "click.html")
        val itr = linkText("Test Invalid Click").findAllElements
        itr.hasNext should be (false)
      }
    }
    
    describe("partialLinkText") {
      it("should return Element correctly when element method is called with valid partialLinkText") {
        go to (host + "click.html")
        partialLinkText("Click").element.isInstanceOf[Element] should be (true)
      }
      it("should throw TestFailedException with correct stack depth and cause when element method is called with invalid partialLinkText", Slow) {
        go to (host + "click.html")
        val e = intercept[TestFailedException] {
          partialLinkText("Click Invalid").element
        }
        e.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        e.getCause.isInstanceOf[org.openqa.selenium.NoSuchElementException] should be (true)
      }
      it("should return Some(Element) when findElement method is called with valid partialLinkText") {
        go to (host + "click.html")
        partialLinkText("Click").findElement match {
          case Some(element: Element) => // ok
          case other => fail("Expected Some(element: Element), but got: " + other)
        }
      }
      it("should return None when findElement method is called with invalid partialLinkText", Slow) {
        go to (host + "click.html")
        partialLinkText("Click Invalid").findElement should be (None)
      }
      it("should return WebElement correctly when webElement method is called with valid partialLinkText") {
        go to (host + "click.html")
        partialLinkText("Click").webElement.isInstanceOf[WebElement] should be (true)
      }
      it("should throw TestFailedException with correct stack depth and cause when webElement method is called with invalid partialLinkText", Slow) {
        go to (host + "click.html")
        val e = intercept[TestFailedException] {
          partialLinkText("Click Invalid").webElement
        }
        e.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        e.getCause.isInstanceOf[org.openqa.selenium.NoSuchElementException] should be (true)
      }
      it("should return list Iterator[Element] with size of 1 when findAllElements is called with valid partialLinkText") {
        go to (host + "click.html")
        val itr = partialLinkText("Click").findAllElements
        itr.hasNext should be (true)
        itr.next.isInstanceOf[Element] should be (true)
        itr.hasNext should be (false)
      }
      it("should return list Iterator[Element] with size of 0 when findAllElements is called with invalid partialLinkText", Slow) {
        go to (host + "click.html")
        val itr = partialLinkText("Click Invalid").findAllElements
        itr.hasNext should be (false)
      }
    }
    
    describe("tagname") {
      it("should return Element correctly when element method is called with valid tagname") {
        go to (host + "click.html")
        tagName("a").element.isInstanceOf[Element] should be (true)
      }
      it("should throw TestFailedException with correct stack depth and cause when element method is called with invalid tagname", Slow) {
        go to (host + "click.html")
        val e = intercept[TestFailedException] {
          tagName("img").element
        }
        e.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        e.getCause.isInstanceOf[org.openqa.selenium.NoSuchElementException] should be (true)
      }
      it("should return Some(Element) when findElement method is called with valid tagname") {
        go to (host + "click.html")
        tagName("a").findElement match {
          case Some(element: Element) => // ok
          case other => fail("Expected Some(element: Element), but got: " + other)
        }
      }
      it("should return None when findElement method is called with invalid tagname", Slow) {
        go to (host + "click.html")
        tagName("img").findElement should be (None)
      }
      it("should return WebElement correctly when webElement method is called with valid tagname") {
        go to (host + "click.html")
        tagName("a").webElement.isInstanceOf[WebElement] should be (true)
      }
      it("should throw TestFailedException with correct stack depth and cause when webElement method is called with invalid tagname", Slow) {
        go to (host + "click.html")
        val e = intercept[TestFailedException] {
          tagName("img").webElement
        }
        e.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        e.getCause.isInstanceOf[org.openqa.selenium.NoSuchElementException] should be (true)
      }
      it("should return list Iterator[Element] with size of 1 when findAllElements is called with valid tagname") {
        go to (host + "click.html")
        val itr = tagName("a").findAllElements
        itr.hasNext should be (true)
        itr.next.isInstanceOf[Element] should be (true)
        itr.hasNext should be (false)
      }
      it("should return list Iterator[Element] with size of 0 when findAllElements is called with invalid tagname", Slow) {
        go to (host + "click.html")
        val itr = tagName("img").findAllElements
        itr.hasNext should be (false)
      }
    }
  }
  
  describe("switchTo") {
    it("should switch frame correctly and throw TFE with valid stack depth if specified frame not found", Slow) {
      goTo(host + "frame.html")
      val win = windowHandle
      switchTo(frame("frame1"))
      switchTo(window(win))
      switchTo(frame("frame2"))
      
      switchTo(window(win))
      switchTo(frame(0))
      switchTo(window(win))
      switchTo(frame(1))
      
      switchTo(window(win))
      switchTo(frame(id("frame1")))
      switchTo(window(win))
      switchTo(frame(id("frame2")))
      
      switchTo(window(win))
      switchTo(frame(id("frame1").element))
      switchTo(window(win))
      switchTo(frame(id("frame2").element))
      
      switchTo(window(win))
      val caught1= intercept[TestFailedException] {
        switchTo(frame("frame3"))
      }
      caught1.failedCodeLineNumber should be (Some(thisLineNumber - 2))
      caught1.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
      
      val caught2 = intercept[TestFailedException] {
        switch to frame(2)
      }
      caught2.failedCodeLineNumber should be (Some(thisLineNumber - 2))
      caught2.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
      
      val caught3 = intercept[TestFailedException] {
        switch to frame(id("text1"))
      }
      caught3.failedCodeLineNumber should be (Some(thisLineNumber - 2))
      caught3.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
    }
    it("should throw TFE with valid stack depth if specified window handle not found") {
      goTo(host + "window.html")
      val handle = windowHandle
      switchTo(window(handle)) // should be ok
      val caught = intercept[TestFailedException] {
        switchTo(window("Something else"))
      }
      caught.failedCodeLineNumber should be (Some(thisLineNumber - 2))
      caught.failedCodeFileName should be (Some("WebBrowserSpec.scala"))
    }
  }
  
  def thisLineNumber = {
    val st = Thread.currentThread.getStackTrace

    if (!st(2).getMethodName.contains("thisLineNumber"))
      st(2).getLineNumber
    else
      st(3).getLineNumber
  }
}

// class ParallelWebBrowserSpec extends WebBrowserSpec with ParallelTestExecution
