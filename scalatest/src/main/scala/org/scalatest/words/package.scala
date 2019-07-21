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
  @deprecated("The org.scalatest.words.BeWord class has been moved and renamed. Please use org.scalatest.matchers.dsl.BeWord instead.")
  type BeWord = org.scalatest.matchers.dsl.BeWord

  /**
   * <strong>The name <code>org.scalatest.words.ContainWord</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ContainWord</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ContainWord class has been moved and renamed. Please use org.scalatest.matchers.dsl.ContainWord instead.")
  type ContainWord = org.scalatest.matchers.dsl.ContainWord

  /**
   * <strong>The name <code>org.scalatest.words.DefinedWord</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.DefinedWord</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.DefinedWord class has been moved and renamed. Please use org.scalatest.matchers.dsl.DefinedWord instead.")
  type DefinedWord = org.scalatest.matchers.dsl.DefinedWord

  /**
   * <strong>The name <code>org.scalatest.words.EmptyWord</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.EmptyWord</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.EmptyWord class has been moved and renamed. Please use org.scalatest.matchers.dsl.EmptyWord instead.")
  type EmptyWord = org.scalatest.matchers.dsl.EmptyWord

  /**
   * <strong>The name <code>org.scalatest.words.EndWithWord</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.EndWithWord</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.EndWithWord class has been moved and renamed. Please use org.scalatest.matchers.dsl.EndWithWord instead.")
  type EndWithWord = org.scalatest.matchers.dsl.EndWithWord

  /**
   * <strong>The name <code>org.scalatest.words.ExistWord</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ExistWord</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ExistWord class has been moved and renamed. Please use org.scalatest.matchers.dsl.ExistWord instead.")
  type ExistWord = org.scalatest.matchers.dsl.ExistWord

  /**
   * <strong>The name <code>org.scalatest.words.FullyMatchWord</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.FullyMatchWord</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.FullyMatchWord class has been moved and renamed. Please use org.scalatest.matchers.dsl.FullyMatchWord instead.")
  type FullyMatchWord = org.scalatest.matchers.dsl.FullyMatchWord

  /**
   * <strong>The name <code>org.scalatest.words.HaveWord</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.HaveWord</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.HaveWord class has been moved and renamed. Please use org.scalatest.matchers.dsl.HaveWord instead.")
  type HaveWord = org.scalatest.matchers.dsl.HaveWord

  /**
   * <strong>The name <code>org.scalatest.words.IncludeWord</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.IncludeWord</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.IncludeWord class has been moved and renamed. Please use org.scalatest.matchers.dsl.IncludeWord instead.")
  type IncludeWord = org.scalatest.matchers.dsl.IncludeWord

  /**
   * <strong>The name <code>org.scalatest.words.LengthWord</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.LengthWord</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.LengthWord class has been moved and renamed. Please use org.scalatest.matchers.dsl.LengthWord instead.")
  type LengthWord = org.scalatest.matchers.dsl.LengthWord

  /**
   * <strong>The name <code>org.scalatest.words.MatchPatternWord</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.MatchPatternWord</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.MatchPatternWord class has been moved and renamed. Please use org.scalatest.matchers.dsl.MatchPatternWord instead.")
  type MatchPatternWord = org.scalatest.matchers.dsl.MatchPatternWord

  /**
   * <strong>The name <code>org.scalatest.words.MatcherWords</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.MatcherWords</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.MatcherWords class has been moved and renamed. Please use org.scalatest.matchers.dsl.MatcherWords instead.")
  type MatcherWords = org.scalatest.matchers.dsl.MatcherWords

  /**
   * <strong>The name <code>org.scalatest.words.NoExceptionWord</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.NoExceptionWord</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.NoExceptionWord class has been moved and renamed. Please use org.scalatest.matchers.dsl.NoExceptionWord instead.")
  type NoExceptionWord = org.scalatest.matchers.dsl.NoExceptionWord

  /**
   * <strong>The name <code>org.scalatest.words.NotWord</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.NotWord</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.NotWord class has been moved and renamed. Please use org.scalatest.matchers.dsl.NotWord instead.")
  type NotWord = org.scalatest.matchers.dsl.NotWord

  /**
   * <strong>The name <code>org.scalatest.words.PleaseUseNoExceptionShouldSyntaxInstead</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.PleaseUseNoExceptionShouldSyntaxInstead</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.PleaseUseNoExceptionShouldSyntaxInstead class has been moved and renamed. Please use org.scalatest.matchers.dsl.PleaseUseNoExceptionShouldSyntaxInstead instead.")
  type PleaseUseNoExceptionShouldSyntaxInstead = org.scalatest.matchers.dsl.PleaseUseNoExceptionShouldSyntaxInstead

  /**
   * <strong>The name <code>org.scalatest.words.ReadableWord</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ReadableWord</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ReadableWord class has been moved and renamed. Please use org.scalatest.matchers.dsl.ReadableWord instead.")
  type ReadableWord = org.scalatest.matchers.dsl.ReadableWord

  /**
   * <strong>The name <code>org.scalatest.words.RegexWithGroups</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.RegexWithGroups</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.RegexWithGroups class has been moved and renamed. Please use org.scalatest.matchers.dsl.RegexWithGroups instead.")
  type RegexWithGroups = org.scalatest.matchers.dsl.RegexWithGroups

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfATypeInvocation</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfATypeInvocation</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfATypeInvocation class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfATypeInvocation instead.")
  type ResultOfATypeInvocation[T] = org.scalatest.matchers.dsl.ResultOfATypeInvocation[T]

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfAWordToAMatcherApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfAWordToAMatcherApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfAWordToAMatcherApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfAWordToAMatcherApplication instead.")
  type ResultOfAWordToAMatcherApplication[T] = org.scalatest.matchers.dsl.ResultOfAWordToAMatcherApplication[T]

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfAWordToBePropertyMatcherApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfAWordToBePropertyMatcherApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfAWordToBePropertyMatcherApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfAWordToBePropertyMatcherApplication instead.")
  type ResultOfAWordToBePropertyMatcherApplication[T] = org.scalatest.matchers.dsl.ResultOfAWordToBePropertyMatcherApplication[T]

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfAWordToSymbolApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfAWordToSymbolApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfAWordToSymbolApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfAWordToSymbolApplication instead.")
  type ResultOfAWordToSymbolApplication = org.scalatest.matchers.dsl.ResultOfAWordToSymbolApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfAllElementsOfApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfAllElementsOfApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfAllElementsOfApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfAllElementsOfApplication instead.")
  type ResultOfAllElementsOfApplication = org.scalatest.matchers.dsl.ResultOfAllElementsOfApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfAllOfApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfAllOfApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfAllOfApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfAllOfApplication instead.")
  type ResultOfAllOfApplication = org.scalatest.matchers.dsl.ResultOfAllOfApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfAnTypeInvocation</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfAnTypeInvocation</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfAnTypeInvocation class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfAnTypeInvocation instead.")
  type ResultOfAnTypeInvocation[T] = org.scalatest.matchers.dsl.ResultOfAnTypeInvocation[T]

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfAnWordToAnMatcherApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfAnWordToAnMatcherApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfAnWordToAnMatcherApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfAnWordToAnMatcherApplication instead.")
  type ResultOfAnWordToAnMatcherApplication[T] = org.scalatest.matchers.dsl.ResultOfAnWordToAnMatcherApplication[T]

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfAnWordToBePropertyMatcherApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfAnWordToBePropertyMatcherApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfAnWordToBePropertyMatcherApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfAnWordToBePropertyMatcherApplication instead.")
  type ResultOfAnWordToBePropertyMatcherApplication[T] = org.scalatest.matchers.dsl.ResultOfAnWordToBePropertyMatcherApplication[T]

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfAnWordToSymbolApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfAnWordToSymbolApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfAnWordToSymbolApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfAnWordToSymbolApplication instead.")
  type ResultOfAnWordToSymbolApplication = org.scalatest.matchers.dsl.ResultOfAnWordToSymbolApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfAtLeastOneElementOfApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfAtLeastOneElementOfApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfAtLeastOneElementOfApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfAtLeastOneElementOfApplication instead.")
  type ResultOfAtLeastOneElementOfApplication = org.scalatest.matchers.dsl.ResultOfAtLeastOneElementOfApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfAtLeastOneOfApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfAtLeastOneOfApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfAtLeastOneOfApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfAtLeastOneOfApplication instead.")
  type ResultOfAtLeastOneOfApplication = org.scalatest.matchers.dsl.ResultOfAtLeastOneOfApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfAtMostOneElementOfApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfAtMostOneElementOfApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfAtMostOneElementOfApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfAtMostOneElementOfApplication instead.")
  type ResultOfAtMostOneElementOfApplication = org.scalatest.matchers.dsl.ResultOfAtMostOneElementOfApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfAtMostOneOfApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfAtMostOneOfApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfAtMostOneOfApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfAtMostOneOfApplication instead.")
  type ResultOfAtMostOneOfApplication = org.scalatest.matchers.dsl.ResultOfAtMostOneOfApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfBeThrownBy</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfBeThrownBy</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfBeThrownBy class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfBeThrownBy instead.")
  type ResultOfBeThrownBy = org.scalatest.matchers.dsl.ResultOfBeThrownBy

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfBeWordForAType</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfBeWordForAType</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfBeWordForAType class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfBeWordForAType instead.")
  type ResultOfBeWordForAType[T] = org.scalatest.matchers.dsl.ResultOfBeWordForAType[T]

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfBeWordForAnType</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfBeWordForAnType</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfBeWordForAnType class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfBeWordForAnType instead.")
  type ResultOfBeWordForAnType[T] = org.scalatest.matchers.dsl.ResultOfBeWordForAnType[T]

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfBeWordForNoException</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfBeWordForNoException</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfBeWordForNoException class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfBeWordForNoException instead.")
  type ResultOfBeWordForNoException = org.scalatest.matchers.dsl.ResultOfBeWordForNoException

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfContainWord</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfContainWord</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfContainWord class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfContainWord instead.")
  type ResultOfContainWord[T] = org.scalatest.matchers.dsl.ResultOfContainWord[T]

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfDefinedAt</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfDefinedAt</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfDefinedAt class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfDefinedAt instead.")
  type ResultOfDefinedAt[T] = org.scalatest.matchers.dsl.ResultOfDefinedAt[T]

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfGreaterThanComparison</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfGreaterThanComparison</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfGreaterThanComparison class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfGreaterThanComparison instead.")
  type ResultOfGreaterThanComparison[T] = org.scalatest.matchers.dsl.ResultOfGreaterThanComparison[T]

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfGreaterThanOrEqualToComparison</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfGreaterThanOrEqualToComparison</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfGreaterThanOrEqualToComparison class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfGreaterThanOrEqualToComparison instead.")
  type ResultOfGreaterThanOrEqualToComparison[T] = org.scalatest.matchers.dsl.ResultOfGreaterThanOrEqualToComparison[T]

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfInOrderApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfInOrderApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfInOrderApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfInOrderApplication instead.")
  type ResultOfInOrderApplication = org.scalatest.matchers.dsl.ResultOfInOrderApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfInOrderElementsOfApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfInOrderElementsOfApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfInOrderElementsOfApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfInOrderElementsOfApplication instead.")
  type ResultOfInOrderElementsOfApplication = org.scalatest.matchers.dsl.ResultOfInOrderElementsOfApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfInOrderOnlyApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfInOrderOnlyApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfInOrderOnlyApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfInOrderOnlyApplication instead.")
  type ResultOfInOrderOnlyApplication = org.scalatest.matchers.dsl.ResultOfInOrderOnlyApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfKeyWordApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfKeyWordApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfKeyWordApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfKeyWordApplication instead.")
  type ResultOfKeyWordApplication = org.scalatest.matchers.dsl.ResultOfKeyWordApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfLengthWordApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfLengthWordApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfLengthWordApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfLengthWordApplication instead.")
  type ResultOfLengthWordApplication = org.scalatest.matchers.dsl.ResultOfLengthWordApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfLessThanComparison</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfLessThanComparison</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfLessThanComparison class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfLessThanComparison instead.")
  type ResultOfLessThanComparison[T] = org.scalatest.matchers.dsl.ResultOfLessThanComparison[T]

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfLessThanOrEqualToComparison</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfLessThanOrEqualToComparison</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfLessThanOrEqualToComparison class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfLessThanOrEqualToComparison instead.")
  type ResultOfLessThanOrEqualToComparison[T] = org.scalatest.matchers.dsl.ResultOfLessThanOrEqualToComparison[T]

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfMessageWordApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfMessageWordApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfMessageWordApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfMessageWordApplication instead.")
  type ResultOfMessageWordApplication = org.scalatest.matchers.dsl.ResultOfMessageWordApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfNoElementsOfApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfNoElementsOfApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfNoElementsOfApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfNoElementsOfApplication instead.")
  type ResultOfNoElementsOfApplication = org.scalatest.matchers.dsl.ResultOfNoElementsOfApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfNoneOfApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfNoneOfApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfNoneOfApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfNoneOfApplication instead.")
  type ResultOfNoneOfApplication = org.scalatest.matchers.dsl.ResultOfNoneOfApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfNotExist</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfNotExist</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfNotExist class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfNotExist instead.")
  type ResultOfNotExist = org.scalatest.matchers.dsl.ResultOfNotExist

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfNotWordForAny</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfNotWordForAny</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfNotWordForAny class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfNotWordForAny instead.")
  type ResultOfNotWordForAny[T] = org.scalatest.matchers.dsl.ResultOfNotWordForAny[T]

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfOfTypeInvocation</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfOfTypeInvocation</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfOfTypeInvocation class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfOfTypeInvocation instead.")
  type ResultOfOfTypeInvocation[T] = org.scalatest.matchers.dsl.ResultOfOfTypeInvocation[T]

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfOneElementOfApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfOneElementOfApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfOneElementOfApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfOneElementOfApplication instead.")
  type ResultOfOneElementOfApplication = org.scalatest.matchers.dsl.ResultOfOneElementOfApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfOneOfApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfOneOfApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfOneOfApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfOneOfApplication instead.")
  type ResultOfOneOfApplication = org.scalatest.matchers.dsl.ResultOfOneOfApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfOnlyApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfOnlyApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfOnlyApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfOnlyApplication instead.")
  type ResultOfOnlyApplication = org.scalatest.matchers.dsl.ResultOfOnlyApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfRegexWordApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfRegexWordApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfRegexWordApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfRegexWordApplication instead.")
  type ResultOfRegexWordApplication = org.scalatest.matchers.dsl.ResultOfRegexWordApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfSizeWordApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfSizeWordApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfSizeWordApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfSizeWordApplication instead.")
  type ResultOfSizeWordApplication = org.scalatest.matchers.dsl.ResultOfSizeWordApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfTheSameElementsAsApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfTheSameElementsAsApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfTheSameElementsAsApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfTheSameElementsAsApplication instead.")
  type ResultOfTheSameElementsAsApplication = org.scalatest.matchers.dsl.ResultOfTheSameElementsAsApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfTheSameElementsInOrderAsApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfTheSameElementsInOrderAsApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfTheSameElementsInOrderAsApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfTheSameElementsInOrderAsApplication instead.")
  type ResultOfTheSameElementsInOrderAsApplication = org.scalatest.matchers.dsl.ResultOfTheSameElementsInOrderAsApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfTheSameInstanceAsApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfTheSameInstanceAsApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfTheSameInstanceAsApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfTheSameInstanceAsApplication instead.")
  type ResultOfTheSameInstanceAsApplication = org.scalatest.matchers.dsl.ResultOfTheSameInstanceAsApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfTheTypeInvocation</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfTheTypeInvocation</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfTheTypeInvocation class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfTheTypeInvocation instead.")
  type ResultOfTheTypeInvocation[T] = org.scalatest.matchers.dsl.ResultOfTheTypeInvocation[T]

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfThrownByApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfThrownByApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfThrownByApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfThrownByApplication instead.")
  type ResultOfThrownByApplication = org.scalatest.matchers.dsl.ResultOfThrownByApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfValueWordApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.ResultOfValueWordApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfValueWordApplication class has been moved and renamed. Please use org.scalatest.matchers.dsl.ResultOfValueWordApplication instead.")
  type ResultOfValueWordApplication = org.scalatest.matchers.dsl.ResultOfValueWordApplication

  /**
   * <strong>The name <code>org.scalatest.words.SizeWord</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.SizeWord</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.SizeWord class has been moved and renamed. Please use org.scalatest.matchers.dsl.SizeWord instead.")
  type SizeWord = org.scalatest.matchers.dsl.SizeWord

  /**
   * <strong>The name <code>org.scalatest.words.SortedWord</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.SortedWord</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.SortedWord class has been moved and renamed. Please use org.scalatest.matchers.dsl.SortedWord instead.")
  type SortedWord = org.scalatest.matchers.dsl.SortedWord

  /**
   * <strong>The name <code>org.scalatest.words.StartWithWord</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.StartWithWord</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.StartWithWord class has been moved and renamed. Please use org.scalatest.matchers.dsl.StartWithWord instead.")
  type StartWithWord = org.scalatest.matchers.dsl.StartWithWord

  /**
   * <strong>The name <code>org.scalatest.words.WritableWord</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.dsl.WritableWord</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.WritableWord class has been moved and renamed. Please use org.scalatest.matchers.dsl.WritableWord instead.")
  type WritableWord = org.scalatest.matchers.dsl.WritableWord

  /**
   * <strong>The name <code>org.scalatest.words.BehaveWord</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.verbs.BehaveWord</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.BehaveWord class has been moved and renamed. Please use org.scalatest.verbs.BehaveWord instead.")
  type BehaveWord = org.scalatest.verbs.BehaveWord

  /**
   * <strong>The name <code>org.scalatest.words.CanVerb</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.verbs.CanVerb</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.CanVerb class has been moved and renamed. Please use org.scalatest.verbs.CanVerb instead.")
  type CanVerb = org.scalatest.verbs.CanVerb

  /**
   * <strong>The name <code>org.scalatest.words.CompileWord</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.verbs.CompileWord</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.CompileWord class has been moved and renamed. Please use org.scalatest.verbs.CompileWord instead.")
  type CompileWord = org.scalatest.verbs.CompileWord

  /**
   * <strong>The name <code>org.scalatest.words.MustVerb</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.verbs.MustVerb</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.MustVerb class has been moved and renamed. Please use org.scalatest.verbs.MustVerb instead.")
  type MustVerb = org.scalatest.verbs.MustVerb

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfAfterWordApplication</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.verbs.ResultOfAfterWordApplication</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfAfterWordApplication class has been moved and renamed. Please use org.scalatest.verbs.ResultOfAfterWordApplication instead.")
  type ResultOfAfterWordApplication = org.scalatest.verbs.ResultOfAfterWordApplication

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfStringPassedToVerb</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.verbs.ResultOfStringPassedToVerb</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfStringPassedToVerb class has been moved and renamed. Please use org.scalatest.verbs.ResultOfStringPassedToVerb instead.")
  type ResultOfStringPassedToVerb = org.scalatest.verbs.ResultOfStringPassedToVerb

  /**
   * <strong>The name <code>org.scalatest.words.ResultOfTaggedAsInvocation</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.verbs.ResultOfTaggedAsInvocation</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ResultOfTaggedAsInvocation class has been moved and renamed. Please use org.scalatest.verbs.ResultOfTaggedAsInvocation instead.")
  type ResultOfTaggedAsInvocation = org.scalatest.verbs.ResultOfTaggedAsInvocation

  /**
   * <strong>The name <code>org.scalatest.words.ShouldVerb</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.verbs.ShouldVerb</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.ShouldVerb class has been moved and renamed. Please use org.scalatest.verbs.ShouldVerb instead.")
  type ShouldVerb = org.scalatest.verbs.ShouldVerb

  /**
   * <strong>The name <code>org.scalatest.words.StringVerbBehaveLikeInvocation</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.verbs.StringVerbBehaveLikeInvocation</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.StringVerbBehaveLikeInvocation class has been moved and renamed. Please use org.scalatest.verbs.StringVerbBehaveLikeInvocation instead.")
  type StringVerbBehaveLikeInvocation = org.scalatest.verbs.StringVerbBehaveLikeInvocation

  /**
   * <strong>The name <code>org.scalatest.words.StringVerbBlockRegistration</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.verbs.StringVerbBlockRegistration</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.StringVerbBlockRegistration class has been moved and renamed. Please use org.scalatest.verbs.StringVerbBlockRegistration instead.")
  type StringVerbBlockRegistration = org.scalatest.verbs.StringVerbBlockRegistration

  /**
   * <strong>The name <code>org.scalatest.words.StringVerbStringInvocation</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.verbs.StringVerbStringInvocation</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.StringVerbStringInvocation class has been moved and renamed. Please use org.scalatest.verbs.StringVerbStringInvocation instead.")
  type StringVerbStringInvocation = org.scalatest.verbs.StringVerbStringInvocation

  /**
   * <strong>The name <code>org.scalatest.words.SubjectWithAfterWordRegistration</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.verbs.SubjectWithAfterWordRegistration</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.SubjectWithAfterWordRegistration class has been moved and renamed. Please use org.scalatest.verbs.SubjectWithAfterWordRegistration instead.")
  type SubjectWithAfterWordRegistration = org.scalatest.verbs.SubjectWithAfterWordRegistration

  /**
   * <strong>The name <code>org.scalatest.words.TypeCheckWord</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.verbs.TypeCheckWord</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.words.TypeCheckWord class has been moved and renamed. Please use org.scalatest.verbs.TypeCheckWord instead.")
  type TypeCheckWord = org.scalatest.verbs.TypeCheckWord
}
