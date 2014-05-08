package funl

import org.scalatest._

import funl.interp.Interpreter._


class Constructs extends FreeSpec with Matchers
{
	"if" in
	{
 		statement( "if true then 3 else 4" ) shouldBe 3
 		statement( "if false then 3" ) shouldBe ()
 		statement( "if false then 3 else 4" ) shouldBe 4
		statement( """
			if true then
				3 """ ) shouldBe 3
		statement( """
			if false then
				3	""" ) shouldBe ()
		statement( """
			if true then
				3
			else
				4	""" ) shouldBe 3
// 		expr(
// 			"""	if false then
// 					begin
// 						3
// 					end
// 					else
// 					begin
// 						4
// 					end	""" ) shouldBe 4
// 		expr(
// 			"""	if true then
// 					begin
// 						3
// 					end
// 					else 4	""" ) shouldBe 3
// 		expr(
// 			"""	if false then
// 					begin
// 						3
// 					end
// 					else 4	""" ) shouldBe 4
// 		expr(
// 			"""	if true then 3 else
// 					begin
// 						4
// 					end	""" ) shouldBe 3
// 		expr(
// 			"""	if false then 3 else
// 					begin
// 						4
// 					end	""" ) shouldBe 4
	}
}