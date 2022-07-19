package io.chrisdavenport.cormorant.enumeratum

import enumeratum.{Enum, EnumEntry}
import io.chrisdavenport.cormorant.implicits._
import io.chrisdavenport.cormorant.{CSV, Get, Put}

class EnumeratumSpec extends munit.FunSuite {

  sealed trait SomeEnum extends EnumEntry
  object SomeEnum extends Enum[SomeEnum] {
    val values: IndexedSeq[SomeEnum] = findValues
    case object Case1 extends SomeEnum
    case object Case2 extends SomeEnum
  }

  test("be able to derive a put for an enum") {
    assertEquals(Put[SomeEnum].put(SomeEnum.Case2), CSV.Field("Case2"))
  }

  test("be able to derive a get for an enum") {
    assertEquals(Get[SomeEnum].get(CSV.Field("Case1")), Right(SomeEnum.Case1))
  }
}
