package io.chrisdavenport.cormorant

import cats.data.NonEmptyList

sealed trait Error extends Exception {
  final override def fillInStackTrace(): Throwable = this
}

final case class ParseFailure(reason: String) extends Error
object ParseFailure {
  def invalidInput(input: String): ParseFailure = 
    ParseFailure(s"Invalid Input: Received $input")
}

final case class DecodeFailure(failures: NonEmptyList[String]) extends Error
object DecodeFailure {
  def singleFailure(reason: String): DecodeFailure = DecodeFailure(NonEmptyList.of(reason))
}