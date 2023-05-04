package com.github.flyingcats.common

import cats.effect.IO
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.StandardOpenOption
import java.nio.file.Files

object Messenger {

  private val debugFileLocation: Option[Path] =
    None
  private val debugEnabled: Boolean = debugFileLocation.isDefined

  private def debugLog(s: String): IO[Unit] =
    IO.fromOption(debugFileLocation)(
      new RuntimeException(
        "attempted to call debugLog method without a debugFileLocation set"
      )
    ).map(dfl =>
      IO {
        Files.write(dfl, s"$s\n".getBytes(), StandardOpenOption.APPEND)
      }
    )

  def logReceived(s: String): IO[Unit] =
    IO.whenA(debugEnabled)(debugLog(s"received: $s"))

  def respond(s: String): IO[Unit] =
    IO.whenA(debugEnabled)(debugLog(s"response: $s")) >> IO.println(s)
}
