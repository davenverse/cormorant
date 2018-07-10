package io.chrisdavenport.cormorant

sealed trait Error extends Exception {
  final override def fillInStackTrace(): Throwable = this
}
object Error {
  final case class ParseFailure(reason: String) extends Error
  object ParseFailure {
    def invalidInput(input: String): ParseFailure = 
      ParseFailure(s"Invalid Input: Received $input")
  }

  final case class DecodeFailure(failure :String) extends Error
  object DecodeFailure {
  }

  final case class PrintFailure(reason: String) extends Error

}

