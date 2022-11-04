package com.agilogy.wapl

import Json.JsonArray

object JsonParser:
  def parseArray(s: String): Either[ParseError, JsonArray] = Right(JsonArray(List.empty))

  def parseToken(token: String)(s: String): Either[ParseError, Unit] =
    if (s == token) Right(()) else Left(ParseError(s, 0, token))

