import org.scalatest.FunSuite

class SetSuite extends FunSuite {
  import SpreadSheet._
  test("parse expression") {
    assert(parseExpression("3") === Literal(3))
    assert(parseExpression("=3") === Literal(3))
    assert(parseExpression("=3*a1") === Multiply(Literal(3), Ref("a1")))
    assert(
      parseExpression("=3*(4+1)") === Multiply(Literal(3),
                                               Add(Literal(4), Literal(1))))
  }

  test("create right expr tree") {
    assert(
      createExprStorage("=A1,2\n1,=B0") === Map("A0" -> Ref("A1"),
                                                "A1" -> Literal(2.0),
                                                "B0" -> Literal(1.0),
                                                "B1" -> Ref("B0")))

  }

  test("trows rec exeption") {
    assertThrows[SpreadSheet.CyclicRefException] {
      calSpreadSheet(createExprStorage("=A1,=A0"))
    }
  }

  test("calc right") {
    val tree = createExprStorage("=A1,2\n1,=45*2/3*4/4*3*(14-13)")
    val cal = calSpreadSheet(tree).mapValues(_.run)
    assert(cal === Map("A0" -> 2.0, "A1" -> 2.0, "B0" -> 1, "B1" -> 90))
  }
  test("Correct csv") {
    val tree = createExprStorage("=A1,2\n1,=45*2/3*4/4*3*(14-13),2,4\n1")
    val cal = calSpreadSheet(tree).mapValues(_.run).toList.toMap
    assert(
      cal === Map("A0" -> 2.0,
                  "A1" -> 2.0,
                  "B0" -> 1.0,
                  "B1" -> 90.0,
                  "B2" -> 2.0,
                  "B3" -> 4.0,
                  "C0" -> 1.0))

    assert(toCsv(cal) === "2.0,2.0,,\n1.0,90.0,2.0,4.0\n1.0,,,")
  }

}
