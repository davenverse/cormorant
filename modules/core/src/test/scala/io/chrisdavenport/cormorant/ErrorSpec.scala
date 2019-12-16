package io.chrisdavenport.cormorant

class ErrorSpec extends org.specs2.mutable.Specification{
  "Error.DecodeFailure" should {
    "toString should work" in {
      Error.DecodeFailure.single("reason").toString()
        .must_===("DecodeFailure(NonEmptyList(reason))")
    }
  }
  "Error.ParseFailure" should {
    "toString should work" in {
      Error.ParseFailure.invalidInput("invalid").toString()
        .must_===("ParseFailure(Invalid Input: Received invalid)")
    }
  }

  "Error.PrintFailure" should {
    "toString should work" in {
      Error.PrintFailure("reason").toString()
        .must_===("PrintFailure(reason)")
    }
  }
}