package com.github.flyingcats.common

import cats.effect.{IO, IOApp}
import io.circe._, io.circe.parser._
import io.circe.syntax._
import cats.syntax.functor._

trait MaelstromMessage {
  val src: String
  val dest: String
  val body: MaelstromMessageBody
}

trait MaelstromMessageBody {
  val messageType: MaelstromMessageType
}

sealed trait MaelstromMessageType
object MaelstromMessageType {
  case object Init extends MaelstromMessageType
  case object InitOk extends MaelstromMessageType
  case object Echo extends MaelstromMessageType
  case object Generate extends MaelstromMessageType

  def getMessageType(
      typeString: String
  ): Either[Throwable, MaelstromMessageType] =
    typeString match {
      case "init"     => Right(Init)
      case "echo"     => Right(Echo)
      case "generate" => Right(Generate)
      case s =>
        Left(new RuntimeException(s"unrecognised Maelstrom message type: $s"))
    }
}

object MaelstromApp {

  def mainLoop(
      decoderLookup: PartialFunction[
        MaelstromMessageType,
        Either[Throwable, Decoder[
          MaelstromMessage
        ]]
      ],
      eventResponse: PartialFunction[MaelstromMessage, IO[Unit]]
  ): IO[Unit] = {

    def getDecoder(mtype: MaelstromMessageType): IO[Decoder[MaelstromMessage]] =
      decoderLookup.lift(mtype) match {
        case None =>
          IO.raiseError(
            new RuntimeException(
              s"Received unexpected message type for the module: $mtype"
            )
          )
        case Some(value) => IO.fromEither(value)
      }

    def respond(message: MaelstromMessage): IO[Unit] =
      eventResponse.lift(message) match {
        case None =>
          IO.raiseError(
            new RuntimeException(
              s"node did not expect to receive message of type: ${message.body.messageType}"
            )
          )
        case Some(value) => value
      }

    for {
      inputString <- IO.readLine
      inputJson <- IO.fromEither(parse(inputString))
      messageTypeString <- IO.fromEither(
        inputJson.hcursor.downField("body").downField("type").as[String]
      )
      messageType <- IO.fromEither(
        MaelstromMessageType.getMessageType(messageTypeString)
      )
      inputDecoder <- getDecoder(messageType)
      inputMessage <- IO.fromEither(inputJson.as(inputDecoder))
      _ <- respond(inputMessage)
    } yield ()
  }

  def buildAppLoop(
      decoderLookup: PartialFunction[
        MaelstromMessageType,
        Either[Throwable, Decoder[
          MaelstromMessage
        ]]
      ],
      eventResponse: PartialFunction[MaelstromMessage, IO[Unit]]
  ): IO[Unit] =
    NodeInit.initialise >> mainLoop(
      decoderLookup,
      eventResponse
    ).foreverM
}
