package io.chrisdavenport.cormorant

import _root_.cats.data._

class CursorSpec extends munit.FunSuite {

  private val secret = "hunter2-should-not-appear-in-errors"

  private val headers =
    CSV.Headers(NonEmptyList.of(CSV.Header("Color"), CSV.Header("Food")))

  private val row =
    CSV.Row(NonEmptyList.of(CSV.Field("Blue"), CSV.Field(secret)))

  private def failureMessage(r: Either[Error.DecodeFailure, CSV.Field]): String =
    r.fold(_.toString, v => fail(s"expected a failure, got $v"))

  test("atHeader failure does not quote the row contents") {
    val message = failureMessage(Cursor.atHeader(CSV.Header("Missing"))(headers, row))
    assert(
      !message.contains(secret),
      s"row data leaked into the decode failure: $message")
  }

  test("atIndex failure does not quote the row contents") {
    val message = failureMessage(Cursor.atIndex(row, 99))
    assert(
      !message.contains(secret),
      s"row data leaked into the decode failure: $message")
  }

  test("failures still say enough to debug with") {
    val message = failureMessage(Cursor.atIndex(row, 99))
    assert(message.contains("99"), s"missing the index: $message")
    assert(message.contains("2"), s"missing the row width: $message")
  }

  test("a present header still resolves") {
    assertEquals(
      Cursor.atHeader(CSV.Header("Color"))(headers, row),
      Right(CSV.Field("Blue")))
  }
}
