package io.chrisdavenport.cormorant.syntax

import io.chrisdavenport.cormorant.{CSV, Put}

trait put {
  implicit class putOps[A: Put](a: A) {
    def field: CSV.Field = Put[A].put(a)
  }
}

object put extends put
