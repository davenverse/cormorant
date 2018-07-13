package io.chrisdavenport.cormorant.syntax

import io.chrisdavenport.cormorant._

trait printer {
  implicit class printerOps(csv: CSV) {
    def print(p: Printer): String = p.print(csv)
  }
}

object printer extends printer
