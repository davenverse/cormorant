package io.chrisdavenport.cormorant.refined

class RefinedSpec extends munit.FunSuite {
  test("be able to derive a put for a class") {
    import _root_.io.chrisdavenport.cormorant._
    import _root_.io.chrisdavenport.cormorant.implicits._

    import eu.timepit.refined._
    import eu.timepit.refined.api.Refined
    import eu.timepit.refined.collection.NonEmpty
    // import eu.timepit.refined.auto._
    // import eu.timepit.refined.numeric._

    // import eu.timepit.refined.boolean._
    // import eu.timepit.refined.char._
    // import eu.timepit.refined.collection._
    // import eu.timepit.refined.generic._
    // import eu.timepit.refined.string._
    // import shapeless.{ ::, HNil }

    val refinedValue: String Refined NonEmpty = refineMV[NonEmpty]("Hello")

    assertEquals(Put[String Refined NonEmpty].put(refinedValue), CSV.Field("Hello"))

  }

  test("be able to derive a get for a class") {
    import _root_.io.chrisdavenport.cormorant._
    import _root_.io.chrisdavenport.cormorant.implicits._

    import eu.timepit.refined._
    import eu.timepit.refined.api.Refined
    import eu.timepit.refined.collection.NonEmpty

    val refinedValue: String Refined NonEmpty = refineMV[NonEmpty]("Hello")
    val csv = CSV.Field("Hello")

    assertEquals(Get[String Refined NonEmpty].get(csv), Right(refinedValue))

  }
}
