package com.agilogy.wapl

import Json.JsonArray

object JsonParser:

  private val whiteSpaceChars = Set(' ', '\n', '\r', '\t')

  val whitespace: Parser[Int] = (s, position) =>
    Right(position + s.substring(position).takeWhile(c => whiteSpaceChars.contains(c)).length)

  val array: Parser[JsonArray] =
    (string("[") ** whitespace ** string("]"))
      .map(_ => JsonArray(List.empty))

