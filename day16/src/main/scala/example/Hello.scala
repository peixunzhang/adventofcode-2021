package example

import better.files.Resource

object Hello extends App {
  def loadData(): String = {
    BigInt(Resource.getAsString("input.txt").stripLineEnd, 16).toString(2)
  }

  def parseMessage(binary: String): (Message, String) = {
    val version = Integer.parseInt(binary.substring(0, 3), 2)
    val typeId: Int = Integer.parseInt(binary.substring(3, 6), 2)

    typeId match {
      case 4 =>
        val (payload, leftover, _) = binary
          .drop(6)
          .grouped(5)
          .foldLeft((List.empty[String], List.empty[String], false)) {
            case ((keep, leftover, false), group) =>
              if (group.head == '1') (group.tail :: keep, leftover, false)
              else (group.tail :: keep, leftover, true)
            case ((keep, leftover, true), group) =>
              (keep, group :: leftover, true)
          }
        val body = Literal(
          java.lang.Long.parseLong(payload.reverse.mkString, 2)
        )
        (Message(version, body), leftover.reverse.mkString)
      case _ =>
        val lengthTypeId = binary(6).asDigit
        val (messages, leftOver) = lengthTypeId match {
          case 0 =>
            val payloadLength = Integer.parseInt(binary.substring(7, 22), 2)
            val messagesBinary = binary.substring(22, 22 + payloadLength)
            val messages = parseManyMessagesNoLeftover(messagesBinary)
            (messages, binary.drop(22 + payloadLength))
          case 1 =>
            val amount = Integer.parseInt(binary.substring(7, 18), 2)
            val messagesBinary = binary.drop(18)
            parseNMessages(messagesBinary, amount)
          case _ =>
            throw new IllegalStateException()
        }
        (Message(version, Operator(typeId, messages)), leftOver)
    }
  }

  def parseManyMessagesNoLeftover(messagesBinary: String): List[Message] = {
    if (messagesBinary.isEmpty()) Nil
    else {
      val (message, left) = parseMessage(messagesBinary)
      message :: parseManyMessagesNoLeftover(left)
    }
  }

  def parseNMessages(
      messagesBinary: String,
      amount: Int,
      acc: List[Message] = Nil
  ): (List[Message], String) = {
    if (amount <= 0)
      (acc.reverse, messagesBinary)
    else {
      val (message, left) = parseMessage(messagesBinary)
      parseNMessages(left, amount - 1, message :: acc)
    }
  }

  val msg = parseMessage(loadData())._1

  println(s"part1: ${msg.sumOfVersions}")
  println(s"part2: ${msg.eval}")
}

final case class Message(version: Int, body: MessageBody) {
  def sumOfVersions: Int =
    body match {
      case Literal(_) => version
      case Operator(_, messages) =>
        messages.map(_.sumOfVersions).sum + version
    }
  def eval: Option[Long] = {
    body match {
      case Literal(data) => Some(data)
      case Operator(typeId, messages) =>
        val result = messages.flatMap(_.eval)
        typeId match {
          case 0 => Some(result.sum)
          case 1 => Some(result.product)
          case 2 => result.minOption
          case 3 => result.maxOption
          case 5 => result match {
            case x :: y :: Nil => if (x > y) Some(1) else Some(0)
            case _ => None
          }
          case 6 => result match {
            case x :: y :: Nil => if (x < y) Some(1) else Some(0)
            case _ => None
          }
           case 7 => result match {
            case x :: y :: Nil => if (x == y) Some(1) else Some(0)
            case _ => None
          }
          case _ => None
        }
    }
  }
}

sealed trait MessageBody
final case class Literal(data: Long) extends MessageBody

final case class Operator(
    typeId: Int,
    messages: List[Message]
) extends MessageBody
