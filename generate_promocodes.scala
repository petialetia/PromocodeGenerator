//> using dep com.softwaremill.sttp.tapir::tapir-core:1.11.7
//> using dep com.softwaremill.sttp.tapir::tapir-http4s-server:1.11.7
//> using dep com.softwaremill.sttp.tapir::tapir-swagger-ui-bundle:1.11.7
//> using dep com.softwaremill.sttp.tapir::tapir-jsoniter-scala:1.11.7
//> using dep org.http4s::http4s-blaze-server:0.23.16
//> using dep com.github.plokhotnyuk.jsoniter-scala::jsoniter-scala-macros:2.31.1

import java.time.LocalDate
import java.time.temporal.ChronoUnit
import scala.collection.mutable.HashMap
import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.std.Queue
import cats.effect.kernel.Ref
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

type ProcessIndex = Int

case class GenerationQueryResult(process_id: ProcessIndex) derives ConfiguredJsonValueCodec, Schema

type Minutes = Int

case class CheckingStatusQueryResult(
  progress_persentages: Int,
  remainig_time_estimation: Option[Minutes]
) derives ConfiguredJsonValueCodec, Schema

case class ProcessInfo(
  n_required_promocodes: Int,
  generated_promocodes: List[String],
  start_time: LocalDate
)

object PromocodeGenerator extends IOApp:
  val MAX_N_PROMOCODES = 100_000_000
  val MAX_N_RANDOM_CHARACTERS = 100
  val CHARACTERS = ('A' to 'Z').toSet ++ ('0' to '9')
  val N_CHARACTERS = CHARACTERS.size
  val N_CAHARACTER_FOR_FIBER_TO_GENERATE = 3

  val free_process_ids = for {
    queue <- Queue.unbounded[IO, ProcessIndex]
    _ <- List.tabulate(Int.MaxValue)(i => i).traverse(queue.offer)
  } yield queue

  val promocodes_infos = for {
    promocodes_infos <- Ref[IO].of(HashMap[ProcessIndex, ProcessInfo]())
  } yield promocodes_infos

  def validateNPromocodes(n_promocodes: Int): Either[Error, Int] = {
    n_promocodes match {
      case value if value <= 0 => Left(Error("n_promocodes must be positive"))
      case value if value > 100_000_000 => Left(Error(s"n_promocodes must be less or equal to ${MAX_N_PROMOCODES}"))
      case _ => Right(n_promocodes)
    }
  }

  def getNVariantsOfPromocodes(n_random_characters: Int) = {
    scala.math.pow(N_CHARACTERS, n_random_characters)
  }

  def validateNRandomCharacters(n_random_characters: Int, n_promocodes: Int): Either[Error, Int] = {
    n_random_characters match {
      case value if value <= 0 => Left(Error("n_random_characters must be positive"))
      case value if value > MAX_N_RANDOM_CHARACTERS => Left(Error(s"n_random_characters must be less or equal to ${MAX_N_RANDOM_CHARACTERS}"))
      case value if getNVariantsOfPromocodes(value) < n_promocodes => Left(Error(s"Too little random characters for ${n_promocodes} promocodes to be generated"))
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

  def generatePromocodes(process_id: ProcessIndex, n_required_promocodes: Int, n_random_characters: Int, common_prefix: String): IO[Unit] = {
    // def generateRoutine(n_required_promocodes: Int, n_random_characters: Int, common_prefix: String): IO[Unit] = {
    //   if (n_required_promocodes < getNVariantsOfPromocodes(N_CAHARACTER_FOR_FIBER_TO_GENERATE)) {
    //     for {
    //       infos <- promocodes_infos
    //       // _ <- infos.modify() //Write generation of combinations of 3 chars and remaining
    //     } yield ()
    //   } else {
    //     val (quotient, remainder) = n_required_promocodes /% N_CHARACTERS

    //     IO(for (i <- (0 to N_CHARACTERS - 1)) {
    //       for {
    //         _ <- generateRoutine(quotient + if (i < remainder) 1 else 0, n_random_characters - 1, common_prefix + CHARACTERS.toList()[i]).start
    //       } yield ()
    //     })
    //   }
    // }

    for {
      infos <- promocodes_infos
      _ <- infos.modify(infos => (infos.addOne(process_id, ProcessInfo(n_required_promocodes, List[String](), java.time.LocalDate.now)), IO))
      // _ <- generateRoutine.start
    } yield ()
  }

  def evalRemainingTime(start_time: LocalDate, current_time: LocalDate, n_required_promocodes: Int, n_generated_promocodes: Int): Option[Int] = {
    n_generated_promocodes match {
      case value if value == 0 => None
      case value if value == n_required_promocodes => None
      case _ => {
        val n_remaining_promocodes_to_generate = n_required_promocodes - n_generated_promocodes
        val average_generation_speed = ChronoUnit.MINUTES.between(start_time, current_time).toDouble / n_generated_promocodes
        Some((n_remaining_promocodes_to_generate / average_generation_speed).toInt)
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
        // current_process_id <- for {
        //   ids_queue <- free_process_ids
        //   current_process_id <- ids_queue.take
        //   _ <-  for {
        //     _ <- generatePromocodes(current_process_id, n_promocodes, n_random_characters, common_prefix).start
        //   } yield()
        // } yield current_process_id
      } yield GenerationQueryResult(n_promocodes))
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
    .serverLogic[IO](process_id =>
      for {
        infos <- promocodes_infos
        infos_unrefed <- infos.get

        result = for {
          process_data <- infos_unrefed.get(process_id).toRight(Error("No started process with such id"))
          n_required_promocodes = process_data.n_required_promocodes
          n_generated_promocodes = process_data.generated_promocodes.size
          start_time = process_data.start_time
          remaining_time = evalRemainingTime(start_time, java.time.LocalDate.now, n_required_promocodes, n_generated_promocodes)
          percentage_of_job_done = (n_generated_promocodes.toDouble / n_required_promocodes * 100).toInt
        } yield CheckingStatusQueryResult(percentage_of_job_done, remaining_time)
      } yield result
    )

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
