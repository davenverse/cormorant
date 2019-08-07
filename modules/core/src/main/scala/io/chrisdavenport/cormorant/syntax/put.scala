package io.chrisdavenport.cormorant.syntax

import io.chrisdavenport.cormorant.{CSV, Put}

trait put {

  implicit class putOps[A](a: A) {

    /**
      * Facilitates the transformation of any `A` with a `Put`
      * instance into a field
      *
      * @example {{{
      * //Before
      * Put[String].put("hello")
      *
      * //After
      * "Hello".field
      * }}}
      */
    def field(implicit P: Put[A]): CSV.Field = P.put(a)

  }

}

object put extends put
