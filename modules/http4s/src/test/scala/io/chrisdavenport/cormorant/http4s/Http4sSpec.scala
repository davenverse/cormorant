package io.chrisdavenport.cormorant.http4s

import io.chrisdavenport.cormorant._
import cats.effect.IO
import org.http4s._
import org.http4s.client._
import org.http4s.dsl.io._
import org.http4s.implicits._
import munit.CatsEffectSuite
import munit.ScalaCheckEffectSuite
import org.scalacheck.effect.PropF
import org.scalacheck.Test.Parameters

class StreamingPrinterSuite
    extends CatsEffectSuite
    with ScalaCheckEffectSuite
    with CormorantArbitraries {

  val minTestsOK = Parameters.default
    .withMinSuccessfulTests(20)
    .withWorkers(2)

  test("Http4s Entity Encoder/Decoder round trip rows") {
    PropF
      .forAllF { (rows: CSV.Rows) =>
        val service = HttpRoutes.of[IO] {
          case _ => Ok(rows)
        }
        val client = Client.fromHttpApp(service.orNotFound)
        client.expect[CSV.Rows]("").map(assertEquals(_, rows))
      }
      .check(minTestsOK)
  }

  test("Http4s Entity Encoder/Decoder round trip complete") {
    PropF
      .forAllF { (rows: CSV.Complete) =>
        val service = HttpRoutes.of[IO] {
          case _ => Ok(rows)
        }
        val client = Client.fromHttpApp(service.orNotFound)
        client.expect[CSV.Complete]("").map(assertEquals(_, rows))
      }
      .check(minTestsOK)
  }
}
