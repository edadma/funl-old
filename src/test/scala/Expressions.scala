package ca.hyperreal.funl

import org.scalatest._
import prop.PropertyChecks

import Evaluator._

/*
class Expressions extends FreeSpec with PropertyChecks with Matchers
{
	"arithmetic" in
	{
		statement( "3 + 4*5" ) shouldBe 23
		statement( "3 + (4*5)" ) shouldBe 23
		statement( "(3 + 4)*5" ) shouldBe 35
//		statement( "a + b", "a" -> 3, "b" -> 4 ) shouldBe 7
	}
	
	"conditional" in
	{
		statement( "5 + (if true then 3 else 4) + 6" ) shouldBe 14
		statement( "5 + (if false then 3 else 4) + 6" ) shouldBe 15
		statement( "5 + (if true then 3) + 6" ) shouldBe 14
		statement( """"5" + (if false then 3) + 6""" ) shouldBe "5()6"
	}
}*/