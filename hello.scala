//> using dep com.softwaremill.sttp.tapir::tapir-core:1.11.5
//> using dep com.softwaremill.sttp.tapir::tapir-http4s-server:1.11.5
//> using dep com.softwaremill.sttp.tapir::tapir-swagger-ui-bundle:1.11.5
//> using dep org.http4s::http4s-blaze-server:0.23.16

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all.*
import org.http4s.HttpRoutes
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.server.Router
import sttp.tapir.*
import sttp.tapir.server.http4s.Http4sServerInterpreter
import sttp.tapir.swagger.bundle.SwaggerInterpreter

object HelloWorldTapir extends IOApp:
  val helloWorldEndpoint = endpoint.get
    .in("hello" / "world")
    .in(query[Int]("name"))
    .out(stringBody)
    .serverLogic[IO](name => IO
      .println(s"Saying hello to: $name")
      .flatMap(_ => IO.pure(Right(s"Hello, $name!"))))

  val helloWorldRoutes: HttpRoutes[IO] = Http4sServerInterpreter[IO]()
    .toRoutes(helloWorldEndpoint)

  val swaggerEndpoints = SwaggerInterpreter()
    .fromServerEndpoints[IO](List(helloWorldEndpoint), "My App", "1.0")

  val swaggerRoutes: HttpRoutes[IO] = Http4sServerInterpreter[IO]().toRoutes(swaggerEndpoints)

  val allRoutes: HttpRoutes[IO] = helloWorldRoutes <+> swaggerRoutes

  override def run(args: List[String]): IO[ExitCode] =
    BlazeServerBuilder[IO]
      .bindHttp(8080, "localhost")
      .withHttpApp(Router("/" -> allRoutes).orNotFound)
      .resource
      .use(_ => IO.never)
      .as(ExitCode.Success)
