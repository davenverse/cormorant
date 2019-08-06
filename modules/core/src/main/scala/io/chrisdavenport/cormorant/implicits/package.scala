package io.chrisdavenport.cormorant

package object implicits
    extends instances.base
    with instances.time
    with syntax.printer
    with syntax.write
    with syntax.labelledwrite
    with syntax.read
    with syntax.labelledread
    with syntax.put
