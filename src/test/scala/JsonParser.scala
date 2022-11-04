package com.agilogy.wapl

import Json.JsonArray

object JsonParser:

  private val whiteSpaceChars = Set(' ', '\n', '\r', '\t')

  val whitespace: Parser[Int] = (s, position) =>
    Right(position + s.substring(position).takeWhile(c => whiteSpaceChars.contains(c)).length)

  val array: Parser[JsonArray] =
    (parseToken("[") ** whitespace ** parseToken("]"))
      .map(_ => JsonArray(List.empty))

  def parseToken(token: String): Parser[Int] = (s, position) =>
    if (s.startsWith(token, position)) Right(position + token.length) else Left(ParseError(s, position, token))

