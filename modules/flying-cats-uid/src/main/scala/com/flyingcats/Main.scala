package com.github.flyingcats.uid

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
case object Init extends MaelstromMessageType
case object InitOk extends MaelstromMessageType
case object Echo extends MaelstromMessageType
case object Generate extends MaelstromMessageType

object MaelstromMessageType {
  def parse(typeString: String): Either[Throwable, MaelstromMessageType] =
    typeString match {
      case "generate" => Right(Generate)
      case s =>
        Left(new RuntimeException(s"unrecognised Maelstrom message type: $s"))
    }
}

case class GenerateMessage(src: String, dest: String, body: GenerateBody)
    extends MaelstromMessage
// case class EchoMessage(src: String, dest: String, body: EchoBody)
// extends MaelstromMessage

case class GenerateBody(
    messageId: Int
) extends MaelstromMessageBody {
  val messageType: MaelstromMessageType = Generate
}
// case class EchoBody(
// messageId: Int,
// echo: String
// ) extends MaelstromMessageBody {
// val messageType: MaelstromMessageType = Echo
// }

case class GenerateResponseMessage(
    src: String,
    dest: String,
    body: GenerateResponseBody
) extends MaelstromMessage

case class GenerateResponseBody(
    messageId: Int,
    inReplyTo: Int,
    id: String
) extends MaelstromMessageBody {
  val messageType: MaelstromMessageType = Generate
}
// case class EchoResponseMessage(
// src: String,
// dest: String,
// body: EchoResponseBody
// ) extends MaelstromMessage

// case class EchoResponseBody(
// messageId: Int,
// inReplyTo: Int,
// echo: String
// ) extends MaelstromMessageBody {
// val messageType: MaelstromMessageType = Echo
// }

object GenerateDecoders {

  def encodeResponseMessage: Encoder[GenerateResponseMessage] =
    new Encoder[GenerateResponseMessage] {
      final def apply(a: GenerateResponseMessage): Json = Json.obj(
        ("src", Json.fromString(a.src)),
        ("dest", Json.fromString(a.dest)),
        (
          "body",
          Json.obj(
            ("type", Json.fromString("generate_ok")),
            ("msg_id", Json.fromInt(a.body.messageId)),
            ("in_reply_to", Json.fromInt(a.body.inReplyTo)),
            ("id", Json.fromString(a.body.id))
          )
        )
      )
    }

  def decodeMessage: Decoder[GenerateMessage] = new Decoder[GenerateMessage] {
    def apply(c: HCursor): Decoder.Result[GenerateMessage] =
      for {
        src <- c.downField("src").as[String]
        dest <- c.downField("dest").as[String]
        body <- c.downField("body").as[GenerateBody]
      } yield GenerateMessage(src, dest, body)
  }

  implicit def decodeBody: Decoder[GenerateBody] = new Decoder[GenerateBody] {
    def apply(c: HCursor): Decoder.Result[GenerateBody] =
      for {
        messageId <- c.downField("msg_id").as[Int]
      } yield GenerateBody(messageId)
  }
}

// object EchoDecoders {

// def encodeResponseMessage: Encoder[EchoResponseMessage] =
// new Encoder[EchoResponseMessage] {
// final def apply(a: EchoResponseMessage): Json = Json.obj(
// ("src", Json.fromString(a.src)),
// ("dest", Json.fromString(a.dest)),
// (
// "body",
// Json.obj(
// ("type", Json.fromString("echo_ok")),
// ("msg_id", Json.fromInt(a.body.messageId)),
// ("in_reply_to", Json.fromInt(a.body.inReplyTo)),
// ("echo", Json.fromString(a.body.echo))
// )
// )
// )
// }

// def decodeMessage: Decoder[EchoMessage] = new Decoder[EchoMessage] {
// def apply(c: HCursor): Decoder.Result[EchoMessage] =
// for {
// src <- c.downField("src").as[String]
// dest <- c.downField("dest").as[String]
// body <- c.downField("body").as[EchoBody]
// } yield EchoMessage(src, dest, body)
// }

// implicit def decodeBody: Decoder[EchoBody] = new Decoder[EchoBody] {
// def apply(c: HCursor): Decoder.Result[EchoBody] =
// for {
// messageId <- c.downField("msg_id").as[Int]
// echo <- c.downField("echo").as[String]
// } yield EchoBody(messageId, echo)
// }
// }

object InitCodecs {

  def encodeResponseMessage: Encoder[InitResponseMessage] =
    new Encoder[InitResponseMessage] {
      final def apply(a: InitResponseMessage): Json = Json.obj(
        ("src", Json.fromString(a.src)),
        ("dest", Json.fromString(a.dest)),
        (
          "body",
          Json.obj(
            ("type", Json.fromString("init_ok")),
            ("in_reply_to", Json.fromInt(1))
          )
        )
      )
    }

  def decodeMessage: Decoder[InitMessage] = new Decoder[InitMessage] {
    def apply(c: HCursor): Decoder.Result[InitMessage] =
      for {
        src <- c.downField("src").as[String]
        dest <- c.downField("dest").as[String]
        body <- c.downField("body").as[InitBody]
      } yield InitMessage(src, dest, body)
  }

  implicit def decodeBody: Decoder[InitBody] = new Decoder[InitBody] {
    def apply(c: HCursor): Decoder.Result[InitBody] =
      for {
        messageId <- c.downField("msg_id").as[Int]
        nodeId <- c.downField("node_id").as[String]
        nodeIds <- c.downField("node_ids").as[Vector[String]]
      } yield InitBody(messageId, nodeId, nodeIds)
  }
}

case class InitMessage(src: String, dest: String, body: InitBody)
    extends MaelstromMessage

case class InitBody(messageId: Int, nodeId: String, NodeIds: Vector[String])
    extends MaelstromMessageBody {
  val messageType: MaelstromMessageType = Init
}

case class InitResponseMessage(
    src: String,
    dest: String,
    body: InitResponseBody
) extends MaelstromMessage

case class InitResponseBody() extends MaelstromMessageBody {
  val messageType: MaelstromMessageType = InitOk
}

case class NodeState(id: String)

object Main extends IOApp.Simple {

  def getUidDecoder(
      messageType: MaelstromMessageType
  ): Either[Throwable, Decoder[MaelstromMessage]] = messageType match {
    case Generate => Right(GenerateDecoders.decodeMessage.widen)
    case v =>
      Left(
        new RuntimeException(
          s"Received unexpected message type for the generate module: $v"
        )
      )
  }
  // def getEchoDecoder(
  // messageType: MaelstromMessageType
  // ): Either[Throwable, Decoder[MaelstromMessage]] = messageType match {
  // case Echo => Right(EchoDecoders.decodeMessage.widen)
  // case v =>
  // Left(
  // new RuntimeException(
  // s"Received unexpected message type for the echo module: $v"
  // )
  // )
  // }

  def generateMessageResponse(echo: MaelstromMessage): IO[Unit] = echo match {
    case GenerateMessage(src, dest, gbody) =>
      IO.println(
        GenerateResponseMessage(
          dest,
          src,
          GenerateResponseBody(
            gbody.messageId,
            gbody.messageId,
            java.util.UUID.randomUUID.toString
          )
        ).asJson(GenerateDecoders.encodeResponseMessage).noSpaces
      )
    case e =>
      IO.raiseError(
        new RuntimeException(
          s"uid node did not expect to receive message of type: $e"
        )
      )
  }
  // def echoMessageResponse(echo: MaelstromMessage): IO[Unit] = echo match {
  // case EchoMessage(src, dest, ebody) =>
  // IO.println(
  // EchoResponseMessage(
  // dest,
  // src,
  // EchoResponseBody(
  // ebody.messageId,
  // ebody.messageId,
  // ebody.echo
  // )
  // ).asJson(EchoDecoders.encodeResponseMessage).noSpaces
  // )
  // case e =>
  // IO.raiseError(
  // new RuntimeException(
  // s"echo node did not expect to receive message of type: $e"
  // )
  // )
  // }

  def nodeInit: IO[NodeState] = for {
    inputString <- IO.readLine
    inputJson <- IO.fromEither(parse(inputString))
    initMessage <- IO.fromEither(
      inputJson.as[InitMessage](InitCodecs.decodeMessage)
    )
    initResponse = InitResponseMessage(
      initMessage.dest,
      initMessage.src,
      InitResponseBody()
    )
    _ <- IO.println(
      initResponse.asJson(InitCodecs.encodeResponseMessage).noSpaces
    )
  } yield NodeState(initMessage.body.nodeId)

  def mainLoop(
      decoderLookup: MaelstromMessageType => Either[Throwable, Decoder[
        MaelstromMessage
      ]],
      eventResponse: MaelstromMessage => IO[Unit]
  ): IO[Unit] = for {
    inputString <- IO.readLine
    inputJson <- IO.fromEither(parse(inputString))
    messageTypeString <- IO.fromEither(
      inputJson.hcursor.downField("body").downField("type").as[String]
    )
    messageType <- IO.fromEither(MaelstromMessageType.parse(messageTypeString))
    inputDecoder <- IO.fromEither(decoderLookup(messageType))
    inputMessage <- IO.fromEither(inputJson.as(inputDecoder))
    _ <- eventResponse(inputMessage)
  } yield ()

  def run: IO[Unit] =
    nodeInit >> mainLoop(
      getUidDecoder,
      generateMessageResponse
    ).foreverM
}
