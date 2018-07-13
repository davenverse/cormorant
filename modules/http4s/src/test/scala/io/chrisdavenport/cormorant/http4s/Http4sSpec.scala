package io.chrisdavenport.cormorant.http4s

import io.chrisdavenport.cormorant._
//import io.chrisdavenport.cormorant.http4s._ // already due to packege
// import _root_.fs2._
import cats.effect.IO
import org.http4s._
import org.http4s.client._
import org.http4s.dsl.io._

class Http4sSpec extends CormorantSpec {
  "Http4s Entity Encoder/Decoder" should {
    "round trip rows" in prop { rows : CSV.Rows => 
      val service = HttpService[IO]{
        case _ => Ok(rows)
      }
      val client = Client.fromHttpService(service)
      client.expect[CSV.Rows]("").unsafeRunSync must_=== rows
    }
    "round trip complete" in prop { rows : CSV.Complete => 
      val service = HttpService[IO]{
        case _ => Ok(rows)
      }
      val client = Client.fromHttpService(service)
      client.expect[CSV.Complete]("").unsafeRunSync must_=== rows
    }
  }
}
