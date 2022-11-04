package com.agilogy.wapl

import Json.*

import org.scalatest.funsuite.AnyFunSuite

import scala.util.Random

class JsonParserTest extends AnyFunSuite :

  import JsonParser.*

  test("Parse empty array") {
    assert(array("[]", 0) == Right(JsonArray(List.empty)))
  }

  private val random = new Random()

  test("Parse empty array with whitespace") {
    val ws = " " * random.between(1, 5)
    assert(array(s"[$ws]", 0) == Right(JsonArray(List.empty)))
  }

  test("Parse empty array failure") {
    val input = "["
    assert(array(input, 0) == Left(ParseError(input, 1, expected = "]")))
  }

  test("Parse token") {
    assert(parseToken("[")("[", 0) == Right(1))
  }

  test("Parse token failure") {
    val input = "notTheStartArrayToken"
    assert(parseToken("[")(input, 0) == Left(ParseError(input, 0, expected = "[")))
  }


