package com.agilogy.wapl

import Json.*

import org.scalatest.funsuite.AnyFunSuite

class JsonParserTest extends AnyFunSuite :

  import JsonParser.*

  test("Parse empty array") {
    assert(array("[]", 0) == Right(JsonArray(List.empty)))
  }

  test("Parse empty array failure") {
    val input = "["
    assert(array(input, 0) == Left(ParseError(input, 1, expected = "]")))
  }

  test("Parse token") {
    assert(parseToken("[")("[", 0) == Right(()))
  }

  test("Parse token failure") {
    val input = "notTheStartArrayToken"
    assert(parseToken("[")(input, 0) == Left(ParseError(input, 0, expected = "[")))
  }


