//> using dep com.softwaremill.sttp.tapir::tapir-core:1.11.5
//> using dep com.softwaremill.sttp.tapir::tapir-http4s-server:1.11.5
//> using dep com.softwaremill.sttp.tapir::tapir-swagger-ui-bundle:1.11.5
//> using dep com.softwaremill.sttp.tapir::tapir-jsoniter-scala:1.11.5
//> using dep org.http4s::http4s-blaze-server:0.23.16
//> using dep com.github.plokhotnyuk.jsoniter-scala::jsoniter-scala-macros:2.30.11

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all.*
import org.http4s.HttpRoutes
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.server.Router
import sttp.tapir.*
import sttp.tapir.server.http4s.Http4sServerInterpreter
import sttp.tapir.swagger.bundle.SwaggerInterpreter
import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*
import sttp.tapir.json.jsoniter.*

case class Error(description: String) derives ConfiguredJsonValueCodec, Schema

case class GenerationQueryInput(
  n_promocodes: Int,
  n_random_characters: Int,
  common_prefix: Option[String]
)

case class GenerationQueryResult(process_id: Int) derives ConfiguredJsonValueCodec, Schema

type Minutes = Int

case class CheckingStatusQueryResult(
  progress_persentages: Int,
  remainig_time_estimation: Option[Minutes]
) derives ConfiguredJsonValueCodec, Schema

object PromocodeGenerator extends IOApp:
  val MAX_N_PROMOCODES = 100_000_000
  val MAX_N_RANDOM_CHARACTERS = 100
  val N_ENGLISH_LETTERS = 26
  val N_DIGITS = 10

  def validateNPromocodes(n_promocodes: Int): Either[Error, Int] = {
    n_promocodes match {
      case value if value <= 0 => Left(Error("n_promocodes must be positive"))
      case value if value > 100_000_000 => Left(Error(s"n_promocodes must be less or equal to ${MAX_N_PROMOCODES}"))
      case _ => Right(n_promocodes)
    }
  }

  def validateNRandomCharacters(n_random_characters: Int, n_promocodes: Int): Either[Error, Int] = {
    n_random_characters match {
      case value if value <= 0 => Left(Error("n_random_characters must be positive"))
      case value if value > MAX_N_RANDOM_CHARACTERS => Left(Error(s"n_random_characters must be less or equal to ${MAX_N_RANDOM_CHARACTERS}"))
      case value if scala.math.pow(N_ENGLISH_LETTERS + N_DIGITS, value) < n_promocodes => Left(Error(s"Too little random characters for ${n_promocodes} promocodes to be generated"))
      case _ => Right(n_random_characters)
    }
  }

  def getCommonPrefix(common_prefix: Option[String]): Either[Error, String] = {
    common_prefix match {
      case None => Right("")
      case Some(common_prefix) =>
        common_prefix match {
          case "" => Left(Error("Common prefix should be not empty string"))
          case _ => Right(common_prefix)
        }
    }
  }

  val generationQueryEndpoint =
    endpoint
    .get
    .in("generate_promocodes")
    .in(query[Int]("n_promocodes"))
    .in(query[Int]("n_random_characters"))
    .in(query[Option[String]]("common_prefix"))
    .mapInTo[GenerationQueryInput]
    .out(jsonBody[GenerationQueryResult])
    .errorOut(jsonBody[Error])
    .serverLogic[IO](input =>
      IO.pure(for {
        n_promocodes <- validateNPromocodes(input.n_promocodes)
        n_random_characters <- validateNRandomCharacters(input.n_random_characters, n_promocodes)
        common_prefix <- getCommonPrefix(input.common_prefix)
      } yield GenerationQueryResult(n_random_characters))
    )

  val generationQueryRoutes: HttpRoutes[IO] = Http4sServerInterpreter[IO]()
    .toRoutes(generationQueryEndpoint)

  val checkingStatusQueryEndpoint =
    endpoint
    .get
    .in("check_status")
    .in(query[Int]("process_id"))
    .out(jsonBody[CheckingStatusQueryResult])
    .errorOut(jsonBody[Error])
    .serverLogic[IO](input => IO
      .println(s"Saying hello to: ${input}")
      .flatMap(_ => IO.pure(Right(CheckingStatusQueryResult(input, None)))))

  val checkingStatusQueryRoutes: HttpRoutes[IO] = Http4sServerInterpreter[IO]()
    .toRoutes(checkingStatusQueryEndpoint)

  val swaggerEndpoints =
    SwaggerInterpreter()
    .fromServerEndpoints[IO](List(generationQueryEndpoint, checkingStatusQueryEndpoint), "My App", "1.0")

  val swaggerRoutes: HttpRoutes[IO] = Http4sServerInterpreter[IO]().toRoutes(swaggerEndpoints)

  val allRoutes: HttpRoutes[IO] = generationQueryRoutes <+> checkingStatusQueryRoutes <+> swaggerRoutes

  override def run(args: List[String]): IO[ExitCode] =
    BlazeServerBuilder[IO]
      .bindHttp(8080, "localhost")
      .withHttpApp(Router("/" -> allRoutes).orNotFound)
      .resource
      .use(_ => IO.never)
      .as(ExitCode.Success)
