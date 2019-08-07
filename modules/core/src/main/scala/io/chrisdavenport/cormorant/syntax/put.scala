package io.chrisdavenport.cormorant.syntax

import io.chrisdavenport.cormorant.{CSV, Put}

trait put {

  implicit class putOps[A](a: A) {

    def field(implicit P: Put[A]): CSV.Field = P.put(a)

  }

}

object put extends put
