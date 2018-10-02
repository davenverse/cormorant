package io.chrisdavenport.cormorant

import cats.data.NonEmptyList
import cats.Semigroup

import cats.implicits._

sealed trait Error extends Exception {
  final override def fillInStackTrace(): Throwable = this
  final override def getMessage: String = toString
}
object Error {
  final case class ParseFailure(reason: String) extends Error
  object ParseFailure {
    def invalidInput(input: String): ParseFailure =
      ParseFailure(s"Invalid Input: Received $input")
  }

  final case class DecodeFailure(failure: NonEmptyList[String]) extends Error {
    override def toString: String = s"DecodeFailure($failure)"
  }
  object DecodeFailure {
    def single(reason: String): DecodeFailure = DecodeFailure(NonEmptyList.of(reason))
    implicit val decodeFailureSemigroup: Semigroup[DecodeFailure] = {
      new Semigroup[DecodeFailure] {
        def combine(x: DecodeFailure, y: DecodeFailure): DecodeFailure =
          DecodeFailure(x.failure |+| y.failure)
      }
    }
  }

  final case class PrintFailure(reason: String) extends Error

}
