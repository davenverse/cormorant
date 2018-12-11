package io.chrisdavenport.cormorant.http4s

import io.chrisdavenport.cormorant._
import cats.effect.IO
import org.http4s._
import org.http4s.client._
import org.http4s.dsl.io._
import org.http4s.implicits._

class Http4sSpec extends CormorantSpec {
  "Http4s Entity Encoder/Decoder" should {
    "round trip rows" in prop { rows : CSV.Rows => 
      val service = HttpRoutes.of[IO] {
        case _ => Ok(rows)
      }
      val client = Client.fromHttpApp(service.orNotFound)
      client.expect[CSV.Rows]("").unsafeRunSync must_=== rows
    }
    "round trip complete" in prop { rows : CSV.Complete => 
      val service = HttpRoutes.of[IO]{
        case _ => Ok(rows)
      }
      val client = Client.fromHttpApp(service.orNotFound)
      client.expect[CSV.Complete]("").unsafeRunSync must_=== rows
    }
  }
  
}
