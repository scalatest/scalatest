package org.scalatest.tools

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FriendlyParamsTranslatorSuite extends FunSuite {
  
  private def parsePropsAndTags(rawargs:String) = {
    val translator = new FriendlyParamsTranslator()
    translator.parsePropsAndTags(Array(rawargs).filter(!_.equals("")))
  }
  
  private def getRepoArgsList(rawargs:String):List[String] = {
    val (propertiesArgsList, includesArgsList, excludesArgsList, repoArgsList, concurrentList, memberOnlyList, wildcardList, 
         suiteList, junitList, testngList) = parsePropsAndTags(rawargs)
    repoArgsList
  }
  
  private def getIncludesArgsList(rawargs:String):List[String] = {
    val (propertiesArgsList, includesArgsList, excludesArgsList, repoArgsList, concurrentList, memberOnlyList, wildcardList, 
         suiteList, junitList, testngList) = parsePropsAndTags(rawargs)
    includesArgsList
  }
  
  private def getExcludesArgsList(rawargs:String):List[String] = {
    val (propertiesArgsList, includesArgsList, excludesArgsList, repoArgsList, concurrentList, memberOnlyList, wildcardList, 
         suiteList, junitList, testngList) = parsePropsAndTags(rawargs)
    excludesArgsList
  }
  
  private def getConcurrent(rawargs:String):Boolean = {
    val (propertiesArgsList, includesArgsList, excludesArgsList, repoArgsList, concurrentList, memberOnlyList, wildcardList, 
         suiteList, junitList, testngList) = parsePropsAndTags(rawargs)
    !concurrentList.isEmpty
  }
  
  private def getMemberOnlyList(rawargs:String):List[String] = {
    val (propertiesArgsList, includesArgsList, excludesArgsList, repoArgsList, concurrentList, memberOnlyList, wildcardList, 
         suiteList, junitList, testngList) = parsePropsAndTags(rawargs)
    memberOnlyList
  }
  
  private def getWildcardList(rawargs:String):List[String] = {
    val (propertiesArgsList, includesArgsList, excludesArgsList, repoArgsList, concurrentList, memberOnlyList, wildcardList, 
         suiteList, junitList, testngList) = parsePropsAndTags(rawargs)
    wildcardList
  }
  
  private def getSuiteList(rawargs:String):List[String] = {
    val (propertiesArgsList, includesArgsList, excludesArgsList, repoArgsList, concurrentList, memberOnlyList, wildcardList, 
         suiteList, junitList, testngList) = parsePropsAndTags(rawargs)
    suiteList
  }
  
  private def getJUnitList(rawargs:String):List[String] = {
    val (propertiesArgsList, includesArgsList, excludesArgsList, repoArgsList, concurrentList, memberOnlyList, wildcardList, 
         suiteList, junitList, testngList) = parsePropsAndTags(rawargs)
    junitList
  }
  
  private def getTestNgList(rawargs:String):List[String] = {
    val (propertiesArgsList, includesArgsList, excludesArgsList, repoArgsList, concurrentList, memberOnlyList, wildcardList, 
         suiteList, junitList, testngList) = parsePropsAndTags(rawargs)
    testngList
  }
  
  // junitxml and file has been disabled until we sort out the way for pararrel execution.
  test("parse argument junitxml(directory=\"test\")") {
    val repoArgsList = getRepoArgsList("junitxml(directory=\"test\")")
    assert(repoArgsList.length == 2)
    assert(repoArgsList(0) == "-u")
    assert(repoArgsList(1) == "test")
  }
  
  test("parse argument junitxml should fail with IllegalArgumentException") {
    intercept[IllegalArgumentException] { getRepoArgsList("junitxml") }
  }
  
  test("parse argument junitxml (directory=\"test\") should fail with IllegalArgumentException") {
    intercept[IllegalArgumentException] { getRepoArgsList("junitxml (directory=\"test\")") }
  }
  
  test("parse argument junitxml directory=\"test\" should fail with IllegalArgumentException") {
    intercept[IllegalArgumentException] { getRepoArgsList("junitxml directory=\"test\"") }
  }
  
  test("parse argument junitxml(directory=\"test\" should fail with IllegalArgumentException") {
    intercept[IllegalArgumentException] { getRepoArgsList("junitxml(directory=\"test\"") }
  }
  
  test("parse argument junitxmldirectory=\"test\") should fail with IllegalArgumentException") {
    intercept[IllegalArgumentException] { getRepoArgsList("junitxmldirectory=\"test\")") }
  }
  
  test("parse argument junitxml(director=\"test\") should fail with IllegalArgumentException") {
    intercept[IllegalArgumentException] { getRepoArgsList("junitxml(director=\"test\")") }
  }
  
  test("parse argument file(filename=\"test.xml\")") {
    val repoArgsList = getRepoArgsList("file(filename=\"test.xml\")")
    assert(repoArgsList.length == 2)
    assert(repoArgsList(0) == "-f")
    assert(repoArgsList(1) == "test.xml")
  }
  
  test("parse argument file(filename=\"test.xml\", config=\"durations shortstacks dropteststarting\")") {
    val repoArgsList = getRepoArgsList("file(filename=\"test.xml\", config=\"durations shortstacks dropteststarting\")")
    assert(repoArgsList.length == 2)
    assert(repoArgsList(0) == "-fDSN")
    assert(repoArgsList(1) == "test.xml")
  }
  
  test("parse argument stdout") {
    val repoArgsList = getRepoArgsList("stdout")
    assert(repoArgsList.length == 1)
    assert(repoArgsList(0) == "-o")
  }
  
  test("parse argument stdout(config=\"nocolor fullstacks doptestsucceeded\")") {
    val repoArgsList = getRepoArgsList("stdout(config=\"nocolor fullstacks droptestsucceeded\")")
    assert(repoArgsList.length == 1)
    assert(repoArgsList(0) == "-oWFC")
  }
  
  test("parse argument stdout (config=\"nocolor fullstacks doptestsucceeded\") should fail with IllegalArgumentException") {
    intercept[IllegalArgumentException] { getRepoArgsList("stdout (config=\"nocolor fullstacks doptestsucceeded\")") }
  }
  
  test("parse argument stdout config=\"nocolor fullstacks doptestsucceeded\" should fail with IllegalArgumentException") {
    intercept[IllegalArgumentException] { getRepoArgsList("stdout config=\"nocolor fullstacks doptestsucceeded\"") }
  }
  
  test("parse argument stdout(config=\"nocolor fullstacks doptestsucceeded\" should fail with IllegalArgumentException") {
    intercept[IllegalArgumentException] { getRepoArgsList("stdout(config=\"nocolor fullstacks doptestsucceeded\"") }
  }
  
  test("parse argument stdoutconfig=\"nocolor fullstacks doptestsucceeded\") should fail with IllegalArgumentException") {
    intercept[IllegalArgumentException] { getRepoArgsList("stdoutconfig=\"nocolor fullstacks doptestsucceeded\")") }
  }
  
  test("parse argument stdout(confi=\"nocolor fullstacks doptestsucceeded\") should fail with IllegalArgumentException") {
    intercept[IllegalArgumentException] { getRepoArgsList("stdout(confi=\"nocolor fullstacks doptestsucceeded\")") }
  }
  
  test("parse argument stderr") {
    val repoArgsList = getRepoArgsList("stderr")
    assert(repoArgsList.length == 1)
    assert(repoArgsList(0) == "-e")
  }
  
  test("parse argument stderr(config=\"dropinfoprovided dropsuitestarting droptestignored\")") {
    val repoArgsList = getRepoArgsList("stderr(config=\"dropinfoprovided dropsuitestarting droptestignored\")")
    assert(repoArgsList.length == 1)
    assert(repoArgsList(0) == "-eOHX")
  }
  
  test("parse argument include(org.scala.a, org.scala.b, org.scala.c)") {
    val inclArgsList = getIncludesArgsList("include(org.scala.a, org.scala.b, org.scala.c)")
    assert(inclArgsList.length == 2)
    assert(inclArgsList(0) == "-n")
    assert(inclArgsList(1) == "org.scala.a org.scala.b org.scala.c")
  }
  
  test("parse argument include(\"org.scala.a\", \"org.scala.b\", \"org.scala.c\")") {
    val inclArgsList = getIncludesArgsList("include(\"org.scala.a\", \"org.scala.b\", \"org.scala.c\")")
    assert(inclArgsList.length == 2)
    assert(inclArgsList(0) == "-n")
    assert(inclArgsList(1) == "org.scala.a org.scala.b org.scala.c")
  }
  
  test("parse argument include should fail with IllegalArgumentException") {
    intercept[IllegalArgumentException] { getIncludesArgsList("include") }
  }
  
  test("parse argument include (org.scala.a, org.scala.b, org.scala.c) should fail with IllegalArgumentException") {
    intercept[IllegalArgumentException] { getIncludesArgsList("include (org.scala.a, org.scala.b, org.scala.c)") }
  }
  
  test("parse argument include(org.scala.a, org.scala.b, org.scala.c should fail with IllegalArgumentException") {
    intercept[IllegalArgumentException] { getIncludesArgsList("include (org.scala.a, org.scala.b, org.scala.c") }
  }
  
  test("parse argument includeorg.scala.a, org.scala.b, org.scala.c) should fail with IllegalArgumentException") {
    intercept[IllegalArgumentException] { getIncludesArgsList("includeorg.scala.a, org.scala.b, org.scala.c)") }
  }
  
  test("parse argument include org.scala.a, org.scala.b, org.scala.c should fail with IllegalArgumentException") {
    intercept[IllegalArgumentException] { getIncludesArgsList("include org.scala.a, org.scala.b, org.scala.c") }
  }
  
  test("parse argument exclude(org.scala.a, org.scala.b, org.scala.c)") {
    val exclArgsList = getExcludesArgsList("exclude(org.scala.a, org.scala.b, org.scala.c)")
    assert(exclArgsList.length == 2)
    assert(exclArgsList(0) == "-l")
    assert(exclArgsList(1) == "org.scala.a org.scala.b org.scala.c")
  }
  
  test("parse argument exclude(\"org.scala.a\", \"org.scala.b\", \"org.scala.c\")") {
    val exclArgsList = getExcludesArgsList("exclude(\"org.scala.a\", \"org.scala.b\", \"org.scala.c\")")
    assert(exclArgsList.length == 2)
    assert(exclArgsList(0) == "-l")
    assert(exclArgsList(1) == "org.scala.a org.scala.b org.scala.c")
  }
  
  test("parse argument graphic") {
    val repoArgsList = getRepoArgsList("graphic")
    assert(repoArgsList.length == 1)
    assert(repoArgsList(0) == "-g")
  }
  
  test("parse argument graphic(config=\"dropinfoprovided dropsuitestarting droptestignored\")") {
    val repoArgsList = getRepoArgsList("graphic(config=\"dropinfoprovided dropsuitestarting droptestignored\")")
    assert(repoArgsList.length == 1)
    assert(repoArgsList(0) == "-gOHX")
  }
  
  test("parse argument graphic(config=\"nocolor\") should fail with IllegalArgumentException") {
    intercept[IllegalArgumentException] { getRepoArgsList("graphic(config=\"nocolor\")") }
  }
  
  test("parse argument graphic(config=\"shortstacks\") should fail with IllegalArgumentException") {
    intercept[IllegalArgumentException] { getRepoArgsList("graphic(config=\"shortstacks\")") }
  }
  
  test("parse argument graphic(config=\"fullstacks\") should fail with IllegalArgumentException") {
    intercept[IllegalArgumentException] { getRepoArgsList("graphic(config=\"fullstacks\")") }
  }
  
  test("parse argument graphic(config=\"durations\") should fail with IllegalArgumentException") {
    intercept[IllegalArgumentException] { getRepoArgsList("graphic(config=\"durations\")") }
  }
  
  // To be enabled when dashboard is supported
  /*test("parse argument dashboard(directory=\"test\")") {
    val repoArgsList = getRepoArgsList("dashboard(directory=\"test\")")
    assert(repoArgsList.length == 2)
    assert(repoArgsList(0) == "-d")
    assert(repoArgsList(1) == "test")
  }
  
  test("parse argument dashboard(directory=\"test\", archive=\"5\")") {
    val repoArgsList = getRepoArgsList("dashboard(directory=\"test\", archive=\"5\")")
    assert(repoArgsList.length == 4)
    assert(repoArgsList(0) == "-d")
    assert(repoArgsList(1) == "test")
    assert(repoArgsList(2) == "-a")
    assert(repoArgsList(3) == "5")
  }
  
  test("parse argument dashboard() show fail with IllegalArgumentException") {
    intercept[IllegalArgumentException] { getRepoArgsList("dashboard()") }
  }
  
  test("parse argument dashboard(directory=\"test\", archive=\"\") show fail with IllegalArgumentException") {
    intercept[IllegalArgumentException] { getRepoArgsList("dashboard(directory=\"test\", archive=\"\")") }
  }*/
  
  ignore("parse argument xml(directory=\"test\")") {
    val repoArgsList = getRepoArgsList("xml(directory=\"test\")")
    assert(repoArgsList.length == 2)
    assert(repoArgsList(0) == "-x")
    assert(repoArgsList(1) == "test")
  }
  
  ignore("parse argument xml should fail with IllegalArgumentException") {
    intercept[IllegalArgumentException] { getRepoArgsList("xml") }
  }
  
  test("parse argument html(directory=\"test\")") {
    val repoArgsList = getRepoArgsList("html(directory=\"test\")")
    assert(repoArgsList.length == 2)
    assert(repoArgsList(0) == "-h")
    assert(repoArgsList(1) == "test")
  }
  
  test("parse argument html(directory=\"test\", css=\"5\")") {
    val repoArgsList = getRepoArgsList("html(directory=\"test\", css=\"mystyles.css\")")
    assert(repoArgsList.length == 4)
    assert(repoArgsList(0) == "-h")
    assert(repoArgsList(1) == "test")
    assert(repoArgsList(2) == "-Y")
    assert(repoArgsList(3) == "mystyles.css")
  }
  
  test("parse argument html() show fail with IllegalArgumentException") {
    intercept[IllegalArgumentException] { getRepoArgsList("html()") }
  }
  
  test("parse argument html(directory=\"test\", css=\"\") show fail with IllegalArgumentException") {
    intercept[IllegalArgumentException] { getRepoArgsList("html(directory=\"test\", css=\"\")") }
  }
  
  test("parse argument reporterclass(classname=\"a.b.c\")") {
    val repoArgsList = getRepoArgsList("reporterclass(classname=\"a.b.c\")")
    assert(repoArgsList.length == 2)
    assert(repoArgsList(0) == "-r")
    assert(repoArgsList(1) == "a.b.c")
  }
  
  test("parse argument reporterclass(classname=\"a.b.c\", config=\"dropsuitestarting shortstacks dropteststarting\")") {
    val repoArgsList = getRepoArgsList("reporterclass(classname=\"a.b.c\", config=\"dropsuitestarting dropinfoprovided dropteststarting\")")
    assert(repoArgsList.length == 2)
    assert(repoArgsList(0) == "-rHON")
    assert(repoArgsList(1) == "a.b.c")
  }
  
  test("parse argument reporterclass should fail with IllegalArgumentException") {
    intercept[IllegalArgumentException] { getRepoArgsList("reporterclass") }
  }
  
  test("parse argument reporterclass(classname=\"a.b.c\", config=\"nocolor\") should fail with IllegalArgumentException") {
    intercept[IllegalArgumentException] { getRepoArgsList("reporterclass(classname=\"a.b.c\", config=\"nocolor\")") }
  }
  
  test("parse argument reporterclass(classname=\"a.b.c\", config=\"shortstacks\") should fail with IllegalArgumentException") {
    intercept[IllegalArgumentException] { getRepoArgsList("reporterclass(classname=\"a.b.c\", config=\"shortstacks\")") }
  }
  
  test("parse argument reporterclass(classname=\"a.b.c\", config=\"fullstacks\") should fail with IllegalArgumentException") {
    intercept[IllegalArgumentException] { getRepoArgsList("reporterclass(classname=\"a.b.c\", config=\"fullstacks\")") }
  }
  
  test("parse argument reporterclass(classname=\"a.b.c\", config=\"durations\") should fail with IllegalArgumentException") {
    intercept[IllegalArgumentException] { getRepoArgsList("reporterclass(classname=\"a.b.c\", config=\"durations\")") }
  }
  
  test("parse argument concurrent") {
    assert(getConcurrent("concurrent"))
    assert(!getConcurrent(""))
  }
  
  test("parse argument membersonly(a.b.c)") {
    val memberOnlyList = getMemberOnlyList("membersonly(a.b.c)")
    assert(memberOnlyList.length == 2)
    assert(memberOnlyList(0) == "-m")
    assert(memberOnlyList(1) == "a.b.c")
  }
  
  test("parse argument membersonly(a.b.c, a.b.d, a.b.e)") {
    val memberOnlyList = getMemberOnlyList("membersonly(a.b.c, a.b.d, a.b.e)")
    assert(memberOnlyList.length == 6)
    assert(memberOnlyList(0) == "-m")
    assert(memberOnlyList(1) == "a.b.c")
    assert(memberOnlyList(2) == "-m")
    assert(memberOnlyList(3) == "a.b.d")
    assert(memberOnlyList(4) == "-m")
    assert(memberOnlyList(5) == "a.b.e")
  }
  
  test("parse argument membersonly should fail with IllegalArgumentException") {
    intercept[IllegalArgumentException] { getMemberOnlyList("membersonly") }
  }
  
  test("parse argument wildcard(a.b.c)") {
    val wildcardList = getWildcardList("wildcard(a.b.c)")
    assert(wildcardList.length == 2)
    assert(wildcardList(0) == "-w")
    assert(wildcardList(1) == "a.b.c")
  }
  
  test("parse argument wildcard(a.b.c, a.b.d, a.b.e)") {
    val wildcardList = getWildcardList("wildcard(a.b.c, a.b.d, a.b.e)")
    assert(wildcardList.length == 6)
    assert(wildcardList(0) == "-w")
    assert(wildcardList(1) == "a.b.c")
    assert(wildcardList(2) == "-w")
    assert(wildcardList(3) == "a.b.d")
    assert(wildcardList(4) == "-w")
    assert(wildcardList(5) == "a.b.e")
  }
  
  test("parse argument wildcard should fail with IllegalArgumentException") {
    intercept[IllegalArgumentException] { getWildcardList("wildcard") }
  }
  
  test("parse argument suite(a.b.c)") {
    val suiteList = getSuiteList("suite(a.b.c)")
    assert(suiteList.length == 2)
    assert(suiteList(0) == "-s")
    assert(suiteList(1) == "a.b.c")
  }
  
  test("parse argument suite(a.b.c, a.b.d, a.b.e)") {
    val suiteList = getSuiteList("suite(a.b.c, a.b.d, a.b.e)")
    assert(suiteList.length == 6)
    assert(suiteList(0) == "-s")
    assert(suiteList(1) == "a.b.c")
    assert(suiteList(2) == "-s")
    assert(suiteList(3) == "a.b.d")
    assert(suiteList(4) == "-s")
    assert(suiteList(5) == "a.b.e")
  }
  
  test("parse argument suite should fail with IllegalArgumentException") {
    intercept[IllegalArgumentException] { getSuiteList("suite") }
  }
  
  test("parse argument junit(a.b.c)") {
    val junitList = getJUnitList("junit(a.b.c)")
    assert(junitList.length == 2)
    assert(junitList(0) == "-j")
    assert(junitList(1) == "a.b.c")
  }
  
  test("parse argument junit(a.b.c, a.b.d, a.b.e)") {
    val junitList = getJUnitList("junit(a.b.c, a.b.d, a.b.e)")
    assert(junitList.length == 6)
    assert(junitList(0) == "-j")
    assert(junitList(1) == "a.b.c")
    assert(junitList(2) == "-j")
    assert(junitList(3) == "a.b.d")
    assert(junitList(4) == "-j")
    assert(junitList(5) == "a.b.e")
  }
  
  test("parse argument junit should fail with IllegalArgumentException") {
    intercept[IllegalArgumentException] { getJUnitList("junit") }
  }
  
  test("parse argument testng(a.b.c)") {
    val testngList = getTestNgList("testng(a.b.c)")
    assert(testngList.length == 2)
    assert(testngList(0) == "-t")
    assert(testngList(1) == "a.b.c")
  }
  
  test("parse argument testng(a.b.c, a.b.d, a.b.e)") {
    val testngList = getTestNgList("testng(a.b.c, a.b.d, a.b.e)")
    assert(testngList.length == 6)
    assert(testngList(0) == "-t")
    assert(testngList(1) == "a.b.c")
    assert(testngList(2) == "-t")
    assert(testngList(3) == "a.b.d")
    assert(testngList(4) == "-t")
    assert(testngList(5) == "a.b.e")
  }
  
  test("parse argument testng should fail with IllegalArgumentException") {
    intercept[IllegalArgumentException] { getTestNgList("testng") }
  }
}
