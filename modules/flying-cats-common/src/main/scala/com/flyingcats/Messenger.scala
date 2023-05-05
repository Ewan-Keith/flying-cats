package com.github.flyingcats.common

import cats.effect.IO
import cats.effect.std.Env
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.StandardOpenOption
import java.nio.file.Files

object Messenger {

  private val debugFileLocation: IO[Option[Path]] =
    Env[IO].get("FLYING_CATS_DEBUG_FILE").map(_.map(s => Paths.get(s)))
  private val debugEnabled: IO[Boolean] = debugFileLocation.map(_.isDefined)

  private def debugLog(s: String): IO[Unit] =
    debugFileLocation.flatMap {
      case None =>
        IO.raiseError(
          new RuntimeException(
            "attempted to call debugLog method without a debugFileLocation set"
          )
        )
      case Some(dfl) =>
        IO {
          Files.write(dfl, s"$s\n".getBytes(), StandardOpenOption.APPEND)
        }.void
    }

  def logReceived(s: String): IO[Unit] =
    debugEnabled.ifM(debugLog(s"received: $s"), IO.unit)

  def sendMessage(s: String): IO[Unit] =
    debugEnabled.ifM(debugLog(s"response: $s"), IO.unit) >> IO.println(s)
}
