//> using dep com.softwaremill.sttp.tapir::tapir-core:1.11.4
//> using dep com.softwaremill.sttp.tapir::tapir-netty-server-sync:1.11.4

import sttp.tapir.*
import sttp.tapir.server.netty.sync.NettySyncServer

@main def helloTapir(): Unit =
  val helloEndpoint = endpoint
    .get
    .in("hello" / "world")
    .in(query[String]("name"))
    .out(stringBody)
    .handleSuccess(name => s"Hello, $name!")

  NettySyncServer()
    .addEndpoint(helloEndpoint)
    .port(8080)
    .startAndWait()
