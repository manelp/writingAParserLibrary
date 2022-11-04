package com.agilogy.wapl

import Json.*

import org.scalatest.funsuite.AnyFunSuite

class JsonParserTest extends AnyFunSuite :

  import JsonParser.*

  test("Parse empty array") {
    assert(parseArray("[]") == JsonArray(List.empty))
  }



