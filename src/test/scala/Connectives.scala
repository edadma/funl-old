package funl

import org.scalatest._
import prop.PropertyChecks

import funl.interp.Interpreter._
import funl.interp.Types


class Connectives extends FreeSpec with PropertyChecks with Matchers with Types
{
	val p =
	Table( "p", true, false )
	val pq =
	Table(
		("p", "q"),
				(true, true),
				(true, false),
				(false, true),
				(false, false)
	)
	val pqr =
	Table(
		("p", "q", "r"),
				(true, true, true),
				(true, true, false),
				(true, false, true),
				(true, false, false),
				(false, true, true),
				(false, true, false),
				(false, false, true),
				(false, false, false)
	)
	
	"boolean" in
	{
		forAll (p) { (p: Boolean) =>
			expression( p.toString, "p" -> p ) shouldBe p
			expression( "not p", "p" -> p ) shouldBe !p
		}
		
		forAll (pq) { (p: Boolean, q: Boolean) =>
			expression( "p or q", "p" -> p, "q" -> q ) shouldBe p || q
			expression( "p and q", "p" -> p, "q" -> q ) shouldBe p && q
			expression( "p xor q", "p" -> p, "q" -> q ) shouldBe p ^ q
		}

		forAll (pqr) { (p: Boolean, q: Boolean, r: Boolean) =>
			expression( "p or q and r", "p" -> p, "q" -> q, "r" -> r ) shouldBe p || (q && r)
			expression( "p and q or r", "p" -> p, "q" -> q, "r" -> r ) shouldBe (p && q) || r
			expression( "p xor q and r", "p" -> p, "q" -> q, "r" -> r ) shouldBe p ^ (q && r)
			expression( "p and q xor r", "p" -> p, "q" -> q, "r" -> r ) shouldBe (p && q) ^ r
		}
	}
	
	"short circuit evaluation" in
	{
		forAll (pq) { (p: Boolean, q: Boolean) =>
			var a = 1
			val _p = (_: List[Any]) => {a *= 2; p}
			val _q = (_: List[Any]) => {a *= 3; q}

			{a = 1; expression( "p() or q()", "p" -> _p, "q" -> _q ); a} shouldBe {a = 1; _p(Nil) || _q(Nil); a}
			{a = 1; expression( "p() and q()", "p" -> _p, "q" -> _q ); a} shouldBe {a = 1; _p(Nil) && _q(Nil); a}
		}

		forAll (pqr) { (p: Boolean, q: Boolean, r: Boolean) =>
			var a = 1
			val _p = (_: List[Any]) => {a *= 2; p}
			val _q = (_: List[Any]) => {a *= 3; q}
			val _r = (_: List[Any]) => {a *= 5; r}

			{a = 1; expression( "p() or q() and r()", "p" -> _p, "q" -> _q, "r" -> _r ); a} shouldBe {a = 1; _p(Nil) || _q(Nil) && _r(Nil); a}
			{a = 1; expression( "p() and q() or r()", "p" -> _p, "q" -> _q, "r" -> _r ); a} shouldBe {a = 1; _p(Nil) && _q(Nil) || _r(Nil); a}
		}
	}
}