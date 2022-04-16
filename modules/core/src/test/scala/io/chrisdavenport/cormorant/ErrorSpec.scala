package io.chrisdavenport.cormorant

class ErrorSpec extends munit.FunSuite {
  test("Error.DecodeFailure toString should work") {
    assertEquals(
      Error.DecodeFailure.single("reason").toString(),
      "DecodeFailure(NonEmptyList(reason))"
    )
  }

  test("Error.ParseFailure toString should work") {
    assertEquals(
      Error.ParseFailure.invalidInput("invalid").toString(),
      "ParseFailure(Invalid Input: Received invalid)"
    )
  }

  test("Error.PrintFailure toString should work") {
    assertEquals(
      Error.PrintFailure("reason").toString(),
      "PrintFailure(reason)"
    )
  }
}
