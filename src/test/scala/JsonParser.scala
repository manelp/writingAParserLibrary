package com.agilogy.wapl

import Json.JsonArray

object JsonParser:
  def parseArray(s: String): Either[ParseError, JsonArray] =
    for
      _ <- parseToken("[")(s, 0)
      _ <- parseToken("]")(s, 1)
    yield JsonArray(List.empty)

  def parseToken(token: String)(s: String, position: Int): Either[ParseError, Unit] =
    if (s.startsWith(token, position)) Right(()) else Left(ParseError(s, position, token))

