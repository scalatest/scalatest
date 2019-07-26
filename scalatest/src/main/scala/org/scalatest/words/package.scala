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

/**
 * A deprecated package that contained support for ScalaTest DSLs.
 */
package object words {

  /**
   * Forces Scaladoc to include the <code>org.scalatest.words</code> package.
   */
  @deprecated("This class is a workaround for a Scaladoc bug. It will be removed in a future version of ScalaTest.")
  final class ForceScaladocGeneration private ()

  /**
   * <strong>The name <code>org.scalatest.words.BeWord</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.BeWord</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.BeWord class has been moved and renamed. Please use org.scalatest.matchers.dsl.BeWord instead.", "3.1.0")
  type BeWord = org.scalatest.matchers.dsl.BeWord

  /**
   * <strong>The name <code>org.scalatest.words.ContainWord</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ContainWord</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ContainWord class has been moved and renamed. Please use org.scalatest.matchers.dsl.ContainWord instead.", "3.1.0")
  type ContainWord = org.scalatest.matchers.dsl.ContainWord

  /**
   * <strong>The name <code>org.scalatest.words.DefinedWord</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.DefinedWord</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.DefinedWord class has been moved and renamed. Please use org.scalatest.matchers.dsl.DefinedWord instead.", "3.1.0")
  type DefinedWord = org.scalatest.matchers.dsl.DefinedWord

  /**
   * <strong>The name <code>org.scalatest.words.EmptyWord</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.EmptyWord</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.EmptyWord class has been moved and renamed. Please use org.scalatest.matchers.dsl.EmptyWord instead.", "3.1.0")
  type EmptyWord = org.scalatest.matchers.dsl.EmptyWord

  /**
   * <strong>The name <code>org.scalatest.words.EndWithWord</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.EndWithWord</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.EndWithWord class has been moved and renamed. Please use org.scalatest.matchers.dsl.EndWithWord instead.", "3.1.0")
  type EndWithWord = org.scalatest.matchers.dsl.EndWithWord

  /**
   * <strong>The name <code>org.scalatest.words.ExistWord</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ExistWord</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ExistWord class has been moved and renamed. Please use org.scalatest.matchers.dsl.ExistWord instead.", "3.1.0")
  type ExistWord = org.scalatest.matchers.dsl.ExistWord

  /**
   * <strong>The name <code>org.scalatest.words.FullyMatchWord</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.FullyMatchWord</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.FullyMatchWord class has been moved and renamed. Please use org.scalatest.matchers.dsl.FullyMatchWord instead.", "3.1.0")
  type FullyMatchWord = org.scalatest.matchers.dsl.FullyMatchWord

  /**
   * <strong>The name <code>org.scalatest.words.HaveWord</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.HaveWord</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.HaveWord class has been moved and renamed. Please use org.scalatest.matchers.dsl.HaveWord instead.", "3.1.0")
  type HaveWord = org.scalatest.matchers.dsl.HaveWord

  /**
   * <strong>The name <code>org.scalatest.words.IncludeWord</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.IncludeWord</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.IncludeWord class has been moved and renamed. Please use org.scalatest.matchers.dsl.IncludeWord instead.", "3.1.0")
  type IncludeWord = org.scalatest.matchers.dsl.IncludeWord

  /**
   * <strong>The name <code>org.scalatest.words.LengthWord</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.LengthWord</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.LengthWord class has been moved and renamed. Please use org.scalatest.matchers.dsl.LengthWord instead.", "3.1.0")
  type LengthWord = org.scalatest.matchers.dsl.LengthWord

  /**
   * <strong>The name <code>org.scalatest.words.MatchPatternWord</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.MatchPatternWord</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.MatchPatternWord class has been moved and renamed. Please use org.scalatest.matchers.dsl.MatchPatternWord instead.", "3.1.0")
  type MatchPatternWord = org.scalatest.matchers.dsl.MatchPatternWord

  /**
   * <strong>The name <code>org.scalatest.words.MatcherWords</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.MatcherWords</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.MatcherWords class has been moved and renamed. Please use org.scalatest.matchers.dsl.MatcherWords instead.", "3.1.0")
  type MatcherWords = org.scalatest.matchers.dsl.MatcherWords

  /**
   * <strong>The name <code>org.scalatest.words.NoExceptionWord</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.NoExceptionWord</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.NoExceptionWord class has been moved and renamed. Please use org.scalatest.matchers.dsl.NoExceptionWord instead.", "3.1.0")
  type NoExceptionWord = org.scalatest.matchers.dsl.NoExceptionWord

  /**
   * <strong>The name <code>org.scalatest.words.NotWord</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.NotWord</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.NotWord class has been moved and renamed. Please use org.scalatest.matchers.dsl.NotWord instead.", "3.1.0")
  type NotWord = org.scalatest.matchers.dsl.NotWord

  /**
   * <strong>The name <code>org.scalatest.words.PleaseUseNoExceptionShouldSyntaxInstead</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.PleaseUseNoExceptionShouldSyntaxInstead</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.PleaseUseNoExceptionShouldSyntaxInstead class has been moved and renamed. Please use org.scalatest.matchers.dsl.PleaseUseNoExceptionShouldSyntaxInstead instead.", "3.1.0")
  type PleaseUseNoExceptionShouldSyntaxInstead = org.scalatest.matchers.dsl.PleaseUseNoExceptionShouldSyntaxInstead

  /**
   * <strong>The name <code>org.scalatest.words.ReadableWord</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ReadableWord</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ReadableWord class has been moved and renamed. Please use org.scalatest.matchers.dsl.ReadableWord instead.", "3.1.0")
  type ReadableWord = org.scalatest.matchers.dsl.ReadableWord

  /**
   * <strong>The name <code>org.scalatest.words.RegexWithGroups</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.RegexWithGroups</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.RegexWithGroups class has been moved and renamed. Please use org.scalatest.matchers.dsl.RegexWithGroups instead.", "3.1.0")
  type RegexWithGroups = org.scalatest.matchers.dsl.RegexWithGroups

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfATypeInvocation</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfATypeInvocation</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfATypeInvocation class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfATypeInvocation instead.", "3.1.0")
  type ResultOfATypeInvocation[T] = org.scalatest.matchers.dsl.ResultOfATypeInvocation[T]

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfAWordToAMatcherApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfAWordToAMatcherApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfAWordToAMatcherApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfAWordToAMatcherApplication instead.", "3.1.0")
  type ResultOfAWordToAMatcherApplication[T] = org.scalatest.matchers.dsl.ResultOfAWordToAMatcherApplication[T]

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfAWordToBePropertyMatcherApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfAWordToBePropertyMatcherApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfAWordToBePropertyMatcherApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfAWordToBePropertyMatcherApplication instead.", "3.1.0")
  type ResultOfAWordToBePropertyMatcherApplication[T] = org.scalatest.matchers.dsl.ResultOfAWordToBePropertyMatcherApplication[T]

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfAWordToSymbolApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfAWordToSymbolApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfAWordToSymbolApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfAWordToSymbolApplication instead.", "3.1.0")
  type ResultOfAWordToSymbolApplication = org.scalatest.matchers.dsl.ResultOfAWordToSymbolApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfAllElementsOfApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfAllElementsOfApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfAllElementsOfApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfAllElementsOfApplication instead.", "3.1.0")
  type ResultOfAllElementsOfApplication = org.scalatest.matchers.dsl.ResultOfAllElementsOfApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfAllOfApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfAllOfApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfAllOfApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfAllOfApplication instead.", "3.1.0")
  type ResultOfAllOfApplication = org.scalatest.matchers.dsl.ResultOfAllOfApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfAnTypeInvocation</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfAnTypeInvocation</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfAnTypeInvocation class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfAnTypeInvocation instead.", "3.1.0")
  type ResultOfAnTypeInvocation[T] = org.scalatest.matchers.dsl.ResultOfAnTypeInvocation[T]

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfAnWordToAnMatcherApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfAnWordToAnMatcherApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfAnWordToAnMatcherApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfAnWordToAnMatcherApplication instead.", "3.1.0")
  type ResultOfAnWordToAnMatcherApplication[T] = org.scalatest.matchers.dsl.ResultOfAnWordToAnMatcherApplication[T]

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfAnWordToBePropertyMatcherApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfAnWordToBePropertyMatcherApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfAnWordToBePropertyMatcherApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfAnWordToBePropertyMatcherApplication instead.", "3.1.0")
  type ResultOfAnWordToBePropertyMatcherApplication[T] = org.scalatest.matchers.dsl.ResultOfAnWordToBePropertyMatcherApplication[T]

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfAnWordToSymbolApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfAnWordToSymbolApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfAnWordToSymbolApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfAnWordToSymbolApplication instead.", "3.1.0")
  type ResultOfAnWordToSymbolApplication = org.scalatest.matchers.dsl.ResultOfAnWordToSymbolApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfAtLeastOneElementOfApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfAtLeastOneElementOfApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfAtLeastOneElementOfApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfAtLeastOneElementOfApplication instead.", "3.1.0")
  type ResultOfAtLeastOneElementOfApplication = org.scalatest.matchers.dsl.ResultOfAtLeastOneElementOfApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfAtLeastOneOfApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfAtLeastOneOfApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfAtLeastOneOfApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfAtLeastOneOfApplication instead.", "3.1.0")
  type ResultOfAtLeastOneOfApplication = org.scalatest.matchers.dsl.ResultOfAtLeastOneOfApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfAtMostOneElementOfApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfAtMostOneElementOfApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfAtMostOneElementOfApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfAtMostOneElementOfApplication instead.", "3.1.0")
  type ResultOfAtMostOneElementOfApplication = org.scalatest.matchers.dsl.ResultOfAtMostOneElementOfApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfAtMostOneOfApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfAtMostOneOfApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfAtMostOneOfApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfAtMostOneOfApplication instead.", "3.1.0")
  type ResultOfAtMostOneOfApplication = org.scalatest.matchers.dsl.ResultOfAtMostOneOfApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfBeThrownBy</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfBeThrownBy</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfBeThrownBy class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfBeThrownBy instead.", "3.1.0")
  type ResultOfBeThrownBy = org.scalatest.matchers.dsl.ResultOfBeThrownBy

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfBeWordForAType</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfBeWordForAType</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfBeWordForAType class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfBeWordForAType instead.", "3.1.0")
  type ResultOfBeWordForAType[T] = org.scalatest.matchers.dsl.ResultOfBeWordForAType[T]

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfBeWordForAnType</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfBeWordForAnType</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfBeWordForAnType class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfBeWordForAnType instead.", "3.1.0")
  type ResultOfBeWordForAnType[T] = org.scalatest.matchers.dsl.ResultOfBeWordForAnType[T]

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfBeWordForNoException</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfBeWordForNoException</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfBeWordForNoException class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfBeWordForNoException instead.", "3.1.0")
  type ResultOfBeWordForNoException = org.scalatest.matchers.dsl.ResultOfBeWordForNoException

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfContainWord</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfContainWord</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfContainWord class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfContainWord instead.", "3.1.0")
  type ResultOfContainWord[T] = org.scalatest.matchers.dsl.ResultOfContainWord[T]

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfDefinedAt</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfDefinedAt</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfDefinedAt class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfDefinedAt instead.", "3.1.0")
  type ResultOfDefinedAt[T] = org.scalatest.matchers.dsl.ResultOfDefinedAt[T]

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfGreaterThanComparison</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfGreaterThanComparison</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfGreaterThanComparison class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfGreaterThanComparison instead.", "3.1.0")
  type ResultOfGreaterThanComparison[T] = org.scalatest.matchers.dsl.ResultOfGreaterThanComparison[T]

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfGreaterThanOrEqualToComparison</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfGreaterThanOrEqualToComparison</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfGreaterThanOrEqualToComparison class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfGreaterThanOrEqualToComparison instead.", "3.1.0")
  type ResultOfGreaterThanOrEqualToComparison[T] = org.scalatest.matchers.dsl.ResultOfGreaterThanOrEqualToComparison[T]

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfInOrderApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfInOrderApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfInOrderApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfInOrderApplication instead.", "3.1.0")
  type ResultOfInOrderApplication = org.scalatest.matchers.dsl.ResultOfInOrderApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfInOrderElementsOfApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfInOrderElementsOfApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfInOrderElementsOfApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfInOrderElementsOfApplication instead.", "3.1.0")
  type ResultOfInOrderElementsOfApplication = org.scalatest.matchers.dsl.ResultOfInOrderElementsOfApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfInOrderOnlyApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfInOrderOnlyApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfInOrderOnlyApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfInOrderOnlyApplication instead.", "3.1.0")
  type ResultOfInOrderOnlyApplication = org.scalatest.matchers.dsl.ResultOfInOrderOnlyApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfKeyWordApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfKeyWordApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfKeyWordApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfKeyWordApplication instead.", "3.1.0")
  type ResultOfKeyWordApplication = org.scalatest.matchers.dsl.ResultOfKeyWordApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfLengthWordApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfLengthWordApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfLengthWordApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfLengthWordApplication instead.", "3.1.0")
  type ResultOfLengthWordApplication = org.scalatest.matchers.dsl.ResultOfLengthWordApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfLessThanComparison</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfLessThanComparison</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfLessThanComparison class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfLessThanComparison instead.", "3.1.0")
  type ResultOfLessThanComparison[T] = org.scalatest.matchers.dsl.ResultOfLessThanComparison[T]

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfLessThanOrEqualToComparison</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfLessThanOrEqualToComparison</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfLessThanOrEqualToComparison class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfLessThanOrEqualToComparison instead.", "3.1.0")
  type ResultOfLessThanOrEqualToComparison[T] = org.scalatest.matchers.dsl.ResultOfLessThanOrEqualToComparison[T]

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfMessageWordApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfMessageWordApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfMessageWordApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfMessageWordApplication instead.", "3.1.0")
  type ResultOfMessageWordApplication = org.scalatest.matchers.dsl.ResultOfMessageWordApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfNoElementsOfApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfNoElementsOfApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfNoElementsOfApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfNoElementsOfApplication instead.", "3.1.0")
  type ResultOfNoElementsOfApplication = org.scalatest.matchers.dsl.ResultOfNoElementsOfApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfNoneOfApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfNoneOfApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfNoneOfApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfNoneOfApplication instead.", "3.1.0")
  type ResultOfNoneOfApplication = org.scalatest.matchers.dsl.ResultOfNoneOfApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfNotExist</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfNotExist</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfNotExist class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfNotExist instead.", "3.1.0")
  type ResultOfNotExist = org.scalatest.matchers.dsl.ResultOfNotExist

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfNotWordForAny</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfNotWordForAny</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfNotWordForAny class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfNotWordForAny instead.", "3.1.0")
  type ResultOfNotWordForAny[T] = org.scalatest.matchers.dsl.ResultOfNotWordForAny[T]

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfOfTypeInvocation</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfOfTypeInvocation</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfOfTypeInvocation class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfOfTypeInvocation instead.", "3.1.0")
  type ResultOfOfTypeInvocation[T] = org.scalatest.matchers.dsl.ResultOfOfTypeInvocation[T]

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfOneElementOfApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfOneElementOfApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfOneElementOfApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfOneElementOfApplication instead.", "3.1.0")
  type ResultOfOneElementOfApplication = org.scalatest.matchers.dsl.ResultOfOneElementOfApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfOneOfApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfOneOfApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfOneOfApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfOneOfApplication instead.", "3.1.0")
  type ResultOfOneOfApplication = org.scalatest.matchers.dsl.ResultOfOneOfApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfOnlyApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfOnlyApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfOnlyApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfOnlyApplication instead.", "3.1.0")
  type ResultOfOnlyApplication = org.scalatest.matchers.dsl.ResultOfOnlyApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfRegexWordApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfRegexWordApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfRegexWordApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfRegexWordApplication instead.", "3.1.0")
  type ResultOfRegexWordApplication = org.scalatest.matchers.dsl.ResultOfRegexWordApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfSizeWordApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfSizeWordApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfSizeWordApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfSizeWordApplication instead.", "3.1.0")
  type ResultOfSizeWordApplication = org.scalatest.matchers.dsl.ResultOfSizeWordApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfTheSameElementsAsApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfTheSameElementsAsApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfTheSameElementsAsApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfTheSameElementsAsApplication instead.", "3.1.0")
  type ResultOfTheSameElementsAsApplication = org.scalatest.matchers.dsl.ResultOfTheSameElementsAsApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfTheSameElementsInOrderAsApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfTheSameElementsInOrderAsApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfTheSameElementsInOrderAsApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfTheSameElementsInOrderAsApplication instead.", "3.1.0")
  type ResultOfTheSameElementsInOrderAsApplication = org.scalatest.matchers.dsl.ResultOfTheSameElementsInOrderAsApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfTheSameInstanceAsApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfTheSameInstanceAsApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfTheSameInstanceAsApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfTheSameInstanceAsApplication instead.", "3.1.0")
  type ResultOfTheSameInstanceAsApplication = org.scalatest.matchers.dsl.ResultOfTheSameInstanceAsApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfTheTypeInvocation</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfTheTypeInvocation</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfTheTypeInvocation class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfTheTypeInvocation instead.", "3.1.0")
  type ResultOfTheTypeInvocation[T] = org.scalatest.matchers.dsl.ResultOfTheTypeInvocation[T]

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfThrownByApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfThrownByApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfThrownByApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfThrownByApplication instead.", "3.1.0")
  type ResultOfThrownByApplication = org.scalatest.matchers.dsl.ResultOfThrownByApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfValueWordApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfValueWordApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfValueWordApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfValueWordApplication instead.", "3.1.0")
  type ResultOfValueWordApplication = org.scalatest.matchers.dsl.ResultOfValueWordApplication

  /**
   * <strong>The name <code>org.scalatest.words.SizeWord</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.SizeWord</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.SizeWord class has been moved and renamed. Please use org.scalatest.matchers.dsl.SizeWord instead.", "3.1.0")
  type SizeWord = org.scalatest.matchers.dsl.SizeWord

  /**
   * <strong>The name <code>org.scalatest.words.SortedWord</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.SortedWord</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.SortedWord class has been moved and renamed. Please use org.scalatest.matchers.dsl.SortedWord instead.", "3.1.0")
  type SortedWord = org.scalatest.matchers.dsl.SortedWord

  /**
   * <strong>The name <code>org.scalatest.words.StartWithWord</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.StartWithWord</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.StartWithWord class has been moved and renamed. Please use org.scalatest.matchers.dsl.StartWithWord instead.", "3.1.0")
  type StartWithWord = org.scalatest.matchers.dsl.StartWithWord

  /**
   * <strong>The name <code>org.scalatest.words.WritableWord</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.WritableWord</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.WritableWord class has been moved and renamed. Please use org.scalatest.matchers.dsl.WritableWord instead.", "3.1.0")
  type WritableWord = org.scalatest.matchers.dsl.WritableWord

  /**
   * <strong>The name <code>org.scalatest.words.BehaveWord</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.verbs.BehaveWord</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.BehaveWord class has been moved and renamed. Please use org.scalatest.verbs.BehaveWord instead.", "3.1.0")
  type BehaveWord = org.scalatest.verbs.BehaveWord

  /**
   * <strong>The name <code>org.scalatest.words.CanVerb</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.verbs.CanVerb</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.CanVerb class has been moved and renamed. Please use org.scalatest.verbs.CanVerb instead.", "3.1.0")
  type CanVerb = org.scalatest.verbs.CanVerb

  /**
   * <strong>The name <code>org.scalatest.words.CompileWord</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.verbs.CompileWord</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.CompileWord class has been moved and renamed. Please use org.scalatest.verbs.CompileWord instead.", "3.1.0")
  type CompileWord = org.scalatest.verbs.CompileWord

  /**
   * <strong>The name <code>org.scalatest.words.MustVerb</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.verbs.MustVerb</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.MustVerb class has been moved and renamed. Please use org.scalatest.verbs.MustVerb instead.", "3.1.0")
  type MustVerb = org.scalatest.verbs.MustVerb

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfAfterWordApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.verbs.ResultOfAfterWordApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfAfterWordApplication class has been moved and renamed. Please use org.scalatest.verbs.ResultOfAfterWordApplication instead.", "3.1.0")
  type ResultOfAfterWordApplication = org.scalatest.verbs.ResultOfAfterWordApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfStringPassedToVerb</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.verbs.ResultOfStringPassedToVerb</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfStringPassedToVerb class has been moved and renamed. Please use org.scalatest.verbs.ResultOfStringPassedToVerb instead.", "3.1.0")
  type ResultOfStringPassedToVerb = org.scalatest.verbs.ResultOfStringPassedToVerb

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfTaggedAsInvocation</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.verbs.ResultOfTaggedAsInvocation</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfTaggedAsInvocation class has been moved and renamed. Please use org.scalatest.verbs.ResultOfTaggedAsInvocation instead.", "3.1.0")
  type ResultOfTaggedAsInvocation = org.scalatest.verbs.ResultOfTaggedAsInvocation

  /**
   * <strong>The name <code>org.scalatest.words.ShouldVerb</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.verbs.ShouldVerb</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ShouldVerb class has been moved and renamed. Please use org.scalatest.verbs.ShouldVerb instead.", "3.1.0")
  type ShouldVerb = org.scalatest.verbs.ShouldVerb

  /**
   * <strong>The name <code>org.scalatest.words.StringVerbBehaveLikeInvocation</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.verbs.StringVerbBehaveLikeInvocation</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.StringVerbBehaveLikeInvocation class has been moved and renamed. Please use org.scalatest.verbs.StringVerbBehaveLikeInvocation instead.", "3.1.0")
  type StringVerbBehaveLikeInvocation = org.scalatest.verbs.StringVerbBehaveLikeInvocation

  /**
   * <strong>The name <code>org.scalatest.words.StringVerbBlockRegistration</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.verbs.StringVerbBlockRegistration</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.StringVerbBlockRegistration class has been moved and renamed. Please use org.scalatest.verbs.StringVerbBlockRegistration instead.", "3.1.0")
  type StringVerbBlockRegistration = org.scalatest.verbs.StringVerbBlockRegistration

  /**
   * <strong>The name <code>org.scalatest.words.StringVerbStringInvocation</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.verbs.StringVerbStringInvocation</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.StringVerbStringInvocation class has been moved and renamed. Please use org.scalatest.verbs.StringVerbStringInvocation instead.", "3.1.0")
  type StringVerbStringInvocation = org.scalatest.verbs.StringVerbStringInvocation

  /**
   * <strong>The name <code>org.scalatest.words.SubjectWithAfterWordRegistration</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.verbs.SubjectWithAfterWordRegistration</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.SubjectWithAfterWordRegistration class has been moved and renamed. Please use org.scalatest.verbs.SubjectWithAfterWordRegistration instead.", "3.1.0")
  type SubjectWithAfterWordRegistration = org.scalatest.verbs.SubjectWithAfterWordRegistration

  /**
   * <strong>The name <code>org.scalatest.words.TypeCheckWord</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.verbs.TypeCheckWord</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.TypeCheckWord class has been moved and renamed. Please use org.scalatest.verbs.TypeCheckWord instead.", "3.1.0")
  type TypeCheckWord = org.scalatest.verbs.TypeCheckWord
}
