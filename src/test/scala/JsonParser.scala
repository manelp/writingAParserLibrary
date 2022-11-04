package com.agilogy.wapl

import Json.JsonArray

object JsonParser:
  val array: Parser[JsonArray] = (s, position) =>
    for
      _ <- parseToken("[")(s, position)
      _ <- parseToken("]")(s, position + 1)
    yield JsonArray(List.empty)

  def parseToken(token: String): Parser[Unit] = (s, position) =>
    if (s.startsWith(token, position)) Right(()) else Left(ParseError(s, position, token))

