package io.chrisdavenport.cormorant

sealed trait Error extends Exception {
  final override def fillInStackTrace(): Throwable = this
}

case class ParseFailure(reason: String) extends Error
object ParseFailure {
  def invalidInput(input: String): ParseFailure = 
    ParseFailure(s"Invalid Input: Received $input")
}
