//> using dep com.softwaremill.sttp.tapir::tapir-core:1.11.4
//> using dep com.softwaremill.sttp.tapir::tapir-netty-server-sync:1.11.4
//> using dep com.softwaremill.sttp.tapir::tapir-swagger-ui-bundle:1.11.4

import sttp.shared.Identity
import sttp.tapir.*
import sttp.tapir.server.netty.sync.NettySyncServer
import sttp.tapir.swagger.bundle.SwaggerInterpreter

@main def tapirDocs(): Unit =
  val e1 = endpoint
    .get
    .in("hello" / "world")
    .in(query[String]("name"))
    .out(stringBody)
    .handleSuccess(name => s"Hello, $name!")

  val e2 = endpoint
    .post
    .in("double")
    .in(stringBody)
    .out(stringBody)
    .errorOut(stringBody)
    .handle { s =>
      s.toIntOption.fold(Left(s"$s is not a number"))(n => Right((n*2).toString))
    }

  val swaggerEndpoints = SwaggerInterpreter()
    .fromServerEndpoints[Identity](List(e1, e2), "My App", "1.0")

  NettySyncServer().port(8080)
    .addEndpoints(List(e1, e2))
    .addEndpoints(swaggerEndpoints)
    .startAndWait()
