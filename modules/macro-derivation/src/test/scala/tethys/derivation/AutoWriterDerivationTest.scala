package tethys.derivation

import org.scalatest.{FlatSpec, Matchers}
import tethys.JsonWriter
import tethys.commons.TokenNode._
import tethys.derivation.ADTWithType._
import tethys.derivation.auto._
import tethys.derivation.semiauto._
import tethys.writers.JsonObjectWriter
import tethys.writers.instances.SimpleJsonObjectWriter
import tethys.writers.tokens.SimpleTokenWriter._

class AutoWriterDerivationTest extends FlatSpec with Matchers {

  behavior of "auto derivation"
  it should "auto derive writer for recursive type" in {
    RecursiveType(1, Seq(RecursiveType(2))).asTokenList shouldBe obj(
      "a" -> 1,
      "children" -> arr(
        obj(
          "a" -> 2,
          "children" -> arr()
        )
      )
    )
  }

  it should "auto derive writer for A => B => A cycle" in {
    ComplexRecursionA(1, Some(ComplexRecursionB(2, ComplexRecursionA(3, None)))).asTokenList shouldBe obj(
      "a" -> 1,
      "b" -> obj(
        "b" -> 2,
        "a" -> obj(
          "a" -> 3
        )
      )
    )
  }

  it should "auto derive writer for sealed cyclic trait with type parameter" in {
    (ADTWithTypeB[Int](1, ADTWithTypeA[Int](2)): ADTWithType[Int]).asTokenList shouldBe obj(
      "a" -> 1,
      "b" -> obj(
        "a" -> 2
      )
    )
  }

  it should "auto derive writer that normally concatenates with other JsonObjectWriter's" in {
    implicit def recursionTraitWithTypeWriter[B: JsonWriter]: JsonObjectWriter[ADTWithType[B]] = {
      val simpleJsonObjectWriter = SimpleJsonObjectWriter[ADTWithType[B]].addField("clazz") {
        case _: ADTWithTypeA[B] => "ADTWithTypeA"
        case _: ADTWithTypeB[B] => "ADTWithTypeB"
      }
      simpleJsonObjectWriter ++ jsonWriter[ADTWithType[B]]
    }

    (ADTWithTypeB[Int](1, ADTWithTypeA[Int](2)): ADTWithType[Int]).asTokenList shouldBe obj(
      "clazz" -> "ADTWithTypeB",
      "a" -> 1,
      "b" -> obj(
        "clazz" -> "ADTWithTypeA",
        "a" -> 2
      )
    )
  }

  it should "not auto derive writer for sealed cyclic trait with type parameter if one of subclasses has additional type" in {
    "tethys.JsonWriter[ADTWithWrongType[Int]]" shouldNot compile
  }
}