//> using dep com.softwaremill.sttp.tapir::tapir-core:1.11.4
//> using dep com.softwaremill.sttp.tapir::tapir-netty-server-sync:1.11.4

import sttp.tapir.*

@main def helloTapir(): Unit =
  val helloEndpoint = endpoint
    .get
    .in("hello" / "world")

  println(helloEndpoint.show)
