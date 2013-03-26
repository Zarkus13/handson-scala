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
package support;

import org.scalatest._
import matchers.{MatchResult, Matcher, Matchers}
import scala.reflect.Manifest
import org.scalatest.verb.ShouldVerb
import scala.collection.Traversable

object Assertions extends Assertions {
  private[support] def areEqualComparingArraysStructurally(left: Any, right: Any) = {
    left match {
      case leftArray: Array[_] =>
        right match {
          case rightArray: Array[_] => leftArray.deep.equals(rightArray.deep)
          case _ => left == right
        }
      case _ => left == right
    }
  }
}

trait HandsonMatchers extends Matchers with ShouldVerb {
  sealed trait Result
  case object Success extends Result
  case object Failure extends Result
  case object Pending extends Result

  private object ShouldMethodHelper {
    def shouldMatcher[T](left: T, rightMatcher: Matcher[T])={
      rightMatcher(left) match {
        case MatchResult(false, failureMessage, _, _, _) => Failure
        case MatchResult(true,_, _, _, _) => Success
        case _ => Pending
      }
    }
  }

  final class AnyShouldWrapper[T](left: T) {

    def should(rightMatcher: Matcher[T])={
      ShouldMethodHelper.shouldMatcher(left, rightMatcher)
    }

    def should(notWord: NotWord) = new ResultOfNotWord[T](left, false)
  }

  final class LengthShouldWrapper[A <: AnyRef <% LengthWrapper](left: A) {

    def should(rightMatcher: Matcher[A])={
      ShouldMethodHelper.shouldMatcher(left, rightMatcher)
    }

    def should(haveWord: HaveWord): ResultOfHaveWordForLengthWrapper[A] =
      new ResultOfHaveWordForLengthWrapper(left, true)

    def should(notWord: NotWord): ResultOfNotWordForLengthWrapper[A] =
      new ResultOfNotWordForLengthWrapper(left, false)

    def should(beWord: BeWord): ResultOfBeWordForAnyRef[A] = new ResultOfBeWordForAnyRef[A](left, true)
  }

  final class SizeShouldWrapper[A <: AnyRef <% SizeWrapper](left: A) {

    def should(rightMatcher: Matcher[A])={
      ShouldMethodHelper.shouldMatcher(left, rightMatcher)
    }

    def should(notWord: NotWord): ResultOfNotWordForSizeWrapper[A] =
      new ResultOfNotWordForSizeWrapper(left, false)

    def should(haveWord: HaveWord): ResultOfHaveWordForSizeWrapper[A] =
      new ResultOfHaveWordForSizeWrapper(left, true)

    def should(beWord: BeWord): ResultOfBeWordForAnyRef[A] = new ResultOfBeWordForAnyRef[A](left, true)
  }

  final class StringShouldWrapper(left: String) extends StringShouldWrapperForVerb(left) {


    def should(rightMatcher: Matcher[String])={
      ShouldMethodHelper.shouldMatcher(left, rightMatcher)
    }

    def should(beWord: BeWord): ResultOfBeWordForAnyRef[String] = new ResultOfBeWordForAnyRef(left, true)

    def should(haveWord: HaveWord): ResultOfHaveWordForString = {
      new ResultOfHaveWordForString(left, true)
    }

    def should(includeWord: IncludeWord): ResultOfIncludeWordForString = {
      new ResultOfIncludeWordForString(left, true)
    }

    def should(startWithWord: StartWithWord): ResultOfStartWithWordForString = {
      new ResultOfStartWithWordForString(left, true)
    }

    def should(endWithWord: EndWithWord): ResultOfEndWithWordForString = {
      new ResultOfEndWithWordForString(left, true)
    }

    def should(fullyMatchWord: FullyMatchWord): ResultOfFullyMatchWordForString = {
      new ResultOfFullyMatchWordForString(left, true)
    }

    def should(notWord: NotWord): ResultOfNotWordForString = {
      new ResultOfNotWordForString(left, false)
    }
  }

  final class DoubleShouldWrapper(left: Double) {

    def should(rightMatcher: Matcher[Double])={
      ShouldMethodHelper.shouldMatcher(left, rightMatcher)
    }

    def should(notWord: NotWord): ResultOfNotWordForDouble = {
      new ResultOfNotWordForDouble(left, false)
    }
  }

  final class FloatShouldWrapper(left: Float) {

    def should(rightMatcher: Matcher[Float])={
      ShouldMethodHelper.shouldMatcher(left, rightMatcher)
    }

    def should(notWord: NotWord): ResultOfNotWordForFloat = {
      new ResultOfNotWordForFloat(left, false)
    }
  }

  final class LongShouldWrapper(left: Long) {

    def should(rightMatcher: Matcher[Long])={
      ShouldMethodHelper.shouldMatcher(left, rightMatcher)
    }

    def should(notWord: NotWord): ResultOfNotWordForLong = {
      new ResultOfNotWordForLong(left, false)
    }
  }

  final class IntShouldWrapper(left: Int) {

    def should(rightMatcher: Matcher[Int])={
      ShouldMethodHelper.shouldMatcher(left, rightMatcher)
    }

    def should(notWord: NotWord): ResultOfNotWordForInt = {
      new ResultOfNotWordForInt(left, false)
    }
  }

  final class ShortShouldWrapper(left: Short) {

    def should(rightMatcher: Matcher[Short])={
      ShouldMethodHelper.shouldMatcher(left, rightMatcher)
    }

    def should(notWord: NotWord): ResultOfNotWordForShort = {
      new ResultOfNotWordForShort(left, false)
    }
  }

  final class ByteShouldWrapper(left: Byte) {

    def should(rightMatcher: Matcher[Byte])={
      ShouldMethodHelper.shouldMatcher(left, rightMatcher)
    }

    def should(notWord: NotWord): ResultOfNotWordForByte = {
      new ResultOfNotWordForByte(left, false)
    }
  }

  final class MapShouldWrapper[K, V](left: scala.collection.Map[K, V]) {

    def should(rightMatcher: Matcher[scala.collection.Map[K, V]])={
      ShouldMethodHelper.shouldMatcher(left, rightMatcher)
    }

    def should(beWord: BeWord): ResultOfBeWordForAnyRef[scala.collection.Map[K, V]] = new ResultOfBeWordForAnyRef(left, true)

    def should(haveWord: HaveWord): ResultOfHaveWordForTraversable[(K, V)] = {
      new ResultOfHaveWordForTraversable(left, true)
    }

    def should(containWord: ContainWord): ResultOfContainWordForMap[K, V] = {
      new ResultOfContainWordForMap(left, true)
    }

    def should(notWord: NotWord): ResultOfNotWordForMap[K, V] = {
      new ResultOfNotWordForMap(left, false)
    }
  }

  final class AnyRefShouldWrapper[T <: AnyRef](left: T) {

    def should(rightMatcher: Matcher[T])={
      ShouldMethodHelper.shouldMatcher(left, rightMatcher)
    }

    def should(notWord: NotWord): ResultOfNotWordForAnyRef[T] =
      new ResultOfNotWordForAnyRef(left, false)

    def should(beWord: BeWord): ResultOfBeWordForAnyRef[T] = new ResultOfBeWordForAnyRef(left, true)
  }

  final class TraversableShouldWrapper[T](left: Traversable[T]) {

    def should(rightMatcher: Matcher[Traversable[T]])={
      ShouldMethodHelper.shouldMatcher(left, rightMatcher)
    }

    def should(haveWord: HaveWord): ResultOfHaveWordForTraversable[T] =
      new ResultOfHaveWordForTraversable(left, true)

    def should(beWord: BeWord): ResultOfBeWordForAnyRef[Traversable[T]] = new ResultOfBeWordForAnyRef(left, true)

    def should(notWord: NotWord): ResultOfNotWordForTraversable[T, Traversable[T]] =
      new ResultOfNotWordForTraversable(left, false)
  }

  final class JavaCollectionShouldWrapper[T](left: java.util.Collection[T]) {

    def should(rightMatcher: Matcher[java.util.Collection[T]])={
      ShouldMethodHelper.shouldMatcher(left, rightMatcher)
    }

    def should(haveWord: HaveWord): ResultOfHaveWordForJavaCollection[T] =
      new ResultOfHaveWordForJavaCollection(left, true)

    def should(beWord: BeWord): ResultOfBeWordForAnyRef[java.util.Collection[T]] = new ResultOfBeWordForAnyRef(left, true)

    def should(notWord: NotWord): ResultOfNotWordForJavaCollection[T, java.util.Collection[T]] =
      new ResultOfNotWordForJavaCollection(left, false)
  }

  final class JavaMapShouldWrapper[K, V](left: java.util.Map[K, V]) {

    def should(rightMatcher: Matcher[java.util.Map[K, V]])={
      ShouldMethodHelper.shouldMatcher(left, rightMatcher)
    }

    def should(containWord: ContainWord): ResultOfContainWordForJavaMap[K, V] = {
      new ResultOfContainWordForJavaMap(left, true)
    }

    def should(haveWord: HaveWord): ResultOfHaveWordForJavaMap = {
      new ResultOfHaveWordForJavaMap(left, true)
    }

    def should(notWord: NotWord): ResultOfNotWordForJavaMap[K, V] = {
      new ResultOfNotWordForJavaMap[K, V](left, false)
    }

    def should(beWord: BeWord): ResultOfBeWordForAnyRef[java.util.Map[K, V]] = new ResultOfBeWordForAnyRef(left, true)
  }

  final class SeqShouldWrapper[T](left: Seq[T]) {

    def should(rightMatcher: Matcher[Seq[T]])={
      ShouldMethodHelper.shouldMatcher(left, rightMatcher)
    }

    def should(haveWord: HaveWord): ResultOfHaveWordForSeq[T] =
      new ResultOfHaveWordForSeq(left, true)

    def should(notWord: NotWord): ResultOfNotWordForAnyRef[Seq[T]] =
      new ResultOfNotWordForAnyRef(left, false)

    def should(beWord: BeWord): ResultOfBeWordForAnyRef[Seq[T]] = new ResultOfBeWordForAnyRef(left, true)
  }

  final class ArrayShouldWrapper[T](left: Array[T]) {

    def should(rightMatcher: Matcher[Array[T]])={
      ShouldMethodHelper.shouldMatcher(left, rightMatcher)
    }

    def should(haveWord: HaveWord): ResultOfHaveWordForSeq[T] = {
      new ResultOfHaveWordForSeq(left, true)
    }

    def should(notWord: NotWord): ResultOfNotWordForArray[T] =
      new ResultOfNotWordForArray(left, false)
  }
  final class ListShouldWrapper[T](left: List[T]) {

    def should(rightMatcher: Matcher[List[T]])={
      ShouldMethodHelper.shouldMatcher(left, rightMatcher)
    }

    def should(beWord: BeWord): ResultOfBeWordForAnyRef[List[T]] = new ResultOfBeWordForAnyRef(left, true)

    def should(haveWord: HaveWord): ResultOfHaveWordForSeq[T] =
      new ResultOfHaveWordForSeq(left, true)

    def should(notWord: NotWord): ResultOfNotWordForSeq[T, List[T]] =
      new ResultOfNotWordForSeq(left, false)
  }

  final class JavaListShouldWrapper[T](left: java.util.List[T]) {

    def should(rightMatcher: Matcher[java.util.List[T]])={
      ShouldMethodHelper.shouldMatcher(left, rightMatcher)
    }

    def should(haveWord: HaveWord): ResultOfHaveWordForJavaList[T] = {
      new ResultOfHaveWordForJavaList(left, true)
    }

    def should(notWord: NotWord): ResultOfNotWordForJavaList[T, java.util.List[T]] = {
      new ResultOfNotWordForJavaList(left, false)
    }
  }

  implicit def convertToAnyShouldWrapper[T](o: T): AnyShouldWrapper[T] = new AnyShouldWrapper(o)

  implicit def convertToDoubleShouldWrapper(o: Double): DoubleShouldWrapper = new DoubleShouldWrapper(o)

  implicit def convertToFloatShouldWrapper(o: Float): FloatShouldWrapper = new FloatShouldWrapper(o)

  implicit def convertToLongShouldWrapper(o: Long): LongShouldWrapper = new LongShouldWrapper(o)

  implicit def convertToIntShouldWrapper(o: Int): IntShouldWrapper = new IntShouldWrapper(o)

  implicit def convertToShortShouldWrapper(o: Short): ShortShouldWrapper = new ShortShouldWrapper(o)

  implicit def convertToByteShouldWrapper(o: Byte): ByteShouldWrapper = new ByteShouldWrapper(o)

  implicit def convertToAnyRefShouldWrapper[T <: AnyRef](o: T): AnyRefShouldWrapper[T] = new AnyRefShouldWrapper[T](o)

  implicit def convertToTraversableShouldWrapper[T](o: Traversable[T]): TraversableShouldWrapper[T] = new TraversableShouldWrapper[T](o)

  implicit def convertToSeqShouldWrapper[T](o: Seq[T]): SeqShouldWrapper[T] = new SeqShouldWrapper[T](o)

  implicit def convertToArrayShouldWrapper[T](o: Array[T]): ArrayShouldWrapper[T] = new ArrayShouldWrapper[T](o)

  implicit def convertToListShouldWrapper[T](o: List[T]): ListShouldWrapper[T] = new ListShouldWrapper[T](o)

  implicit def convertToMapShouldWrapper[K, V](o: scala.collection.Map[K, V]): MapShouldWrapper[K, V] = new MapShouldWrapper[K, V](o)

  implicit override def convertToStringShouldWrapper(o: String): StringShouldWrapper = new StringShouldWrapper(o)

  implicit def convertToJavaCollectionShouldWrapper[T](o: java.util.Collection[T]): JavaCollectionShouldWrapper[T] = new JavaCollectionShouldWrapper[T](o)

  implicit def convertToJavaListShouldWrapper[T](o: java.util.List[T]): JavaListShouldWrapper[T] = new JavaListShouldWrapper[T](o)

  implicit def convertToJavaMapShouldWrapper[K, V](o: java.util.Map[K, V]): JavaMapShouldWrapper[K, V] = new JavaMapShouldWrapper[K, V](o)

  implicit def convertHasIntGetLengthMethodToLengthShouldWrapper[T <: AnyRef { def getLength(): Int}](o: T): LengthShouldWrapper[T] = new LengthShouldWrapper[T](o)

  implicit def convertHasIntGetLengthParameterlessMethodToLengthShouldWrapper[T <: AnyRef { def getLength: Int}](o: T): LengthShouldWrapper[T] = new LengthShouldWrapper[T](o)

  implicit def convertHasIntGetLengthFieldToLengthShouldWrapper[T <: AnyRef { val getLength: Int}](o: T): LengthShouldWrapper[T] = new LengthShouldWrapper[T](o)

  implicit def convertHasIntLengthFieldToLengthShouldWrapper[T <: AnyRef { val length: Int}](o: T): LengthShouldWrapper[T] = new LengthShouldWrapper[T](o)

  implicit def convertHasIntLengthMethodToLengthShouldWrapper[T <: AnyRef { def length(): Int}](o: T): LengthShouldWrapper[T] = new LengthShouldWrapper[T](o)

  implicit def convertHasIntParameterlessLengthMethodToLengthShouldWrapper[T <: AnyRef { def length: Int}](o: T): LengthShouldWrapper[T] = new LengthShouldWrapper[T](o)

  implicit def convertHasLongGetLengthMethodToLengthShouldWrapper[T <: AnyRef { def getLength(): Long}](o: T): LengthShouldWrapper[T] = new LengthShouldWrapper[T](o)

  implicit def convertHasLongGetLengthParameterlessMethodToLengthShouldWrapper[T <: AnyRef { def getLength: Long}](o: T): LengthShouldWrapper[T] = new LengthShouldWrapper[T](o)

  implicit def convertHasLongGetLengthFieldToLengthShouldWrapper[T <: AnyRef { val getLength: Long}](o: T): LengthShouldWrapper[T] = new LengthShouldWrapper[T](o)

  implicit def convertHasLongLengthFieldToLengthShouldWrapper[T <: AnyRef { val length: Long}](o: T): LengthShouldWrapper[T] = new LengthShouldWrapper[T](o)

  implicit def convertHasLongLengthMethodToLengthShouldWrapper[T <: AnyRef { def length(): Long}](o: T): LengthShouldWrapper[T] = new LengthShouldWrapper[T](o)

  implicit def convertHasLongLengthParameterlessMethodToLengthShouldWrapper[T <: AnyRef { def length: Long}](o: T): LengthShouldWrapper[T] = new LengthShouldWrapper[T](o)

  implicit def convertHasIntGetSizeMethodToSizeShouldWrapper[T <: AnyRef { def getSize(): Int}](o: T): SizeShouldWrapper[T] = new SizeShouldWrapper[T](o)

  implicit def convertHasIntGetSizeParameterlessMethodToSizeShouldWrapper[T <: AnyRef { def getSize: Int}](o: T): SizeShouldWrapper[T] = new SizeShouldWrapper[T](o)

  implicit def convertHasIntGetSizeFieldToSizeShouldWrapper[T <: AnyRef { val getSize: Int}](o: T): SizeShouldWrapper[T] = new SizeShouldWrapper[T](o)

  implicit def convertHasIntSizeFieldToSizeShouldWrapper[T <: AnyRef { val size: Int}](o: T): SizeShouldWrapper[T] = new SizeShouldWrapper[T](o)

  implicit def convertHasIntSizeMethodToSizeShouldWrapper[T <: AnyRef { def size(): Int}](o: T): SizeShouldWrapper[T] = new SizeShouldWrapper[T](o)

  implicit def convertHasIntSizeParameterlessMethodToSizeShouldWrapper[T <: AnyRef { def size: Int}](o: T): SizeShouldWrapper[T] = new SizeShouldWrapper[T](o)

  implicit def convertHasLongGetSizeMethodToSizeShouldWrapper[T <: AnyRef { def getSize(): Long}](o: T): SizeShouldWrapper[T] = new SizeShouldWrapper[T](o)

  implicit def convertHasLongGetSizeParameterlessMethodToSizeShouldWrapper[T <: AnyRef { def getSize: Long}](o: T): SizeShouldWrapper[T] = new SizeShouldWrapper[T](o)

  implicit def convertHasLongGetSizeFieldToSizeShouldWrapper[T <: AnyRef { val getSize: Long}](o: T): SizeShouldWrapper[T] = new SizeShouldWrapper[T](o)

  implicit def convertHasLongSizeFieldToSizeShouldWrapper[T <: AnyRef { val size: Long}](o: T): SizeShouldWrapper[T] = new SizeShouldWrapper[T](o)

  implicit def convertHasLongSizeMethodToSizeShouldWrapper[T <: AnyRef { def size(): Long}](o: T): SizeShouldWrapper[T] = new SizeShouldWrapper[T](o)

  implicit def convertHasLongSizeParameterlessMethodToSizeShouldWrapper[T <: AnyRef { def size: Long}](o: T): SizeShouldWrapper[T] = new SizeShouldWrapper[T](o)
}

object HandsonMatchers extends HandsonMatchers
