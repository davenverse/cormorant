package io.chrisdavenport.cormorant.refined

import org.specs2._

class RefinedSpec extends mutable.Specification {
  "refined module" should {
    "be able to derive a put for a class" in {
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

      val refinedValue : String Refined NonEmpty = refineMV[NonEmpty]("Hello")

        Put[String Refined NonEmpty].put(refinedValue) must_=== CSV.Field("Hello")

    }

    "be able to derive a get for a class" in {
      import _root_.io.chrisdavenport.cormorant._
      import _root_.io.chrisdavenport.cormorant.implicits._

      import eu.timepit.refined._
      import eu.timepit.refined.api.Refined
      import eu.timepit.refined.collection.NonEmpty

      val refinedValue : String Refined NonEmpty = refineMV[NonEmpty]("Hello")
      val csv = CSV.Field("Hello")

      Get[String Refined NonEmpty].get(csv) must_=== Right(refinedValue)

    }
  }
}