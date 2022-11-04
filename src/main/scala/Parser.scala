package com.agilogy.wapl

type Parser[A] = (String, Int) => Either[ParseError, A]