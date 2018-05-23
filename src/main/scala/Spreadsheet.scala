object SpreadSheet {

  val alphabet = 'A' to 'Z'
  val indexes = alphabet.zipWithIndex.toMap

  def toCsv(calculated: Map[String, Double]) = {
    val height = calculated.par.keys.map(x => indexes(x(0))).max + 1
    val width = calculated.par.keys.map(x => x.drop(1).toInt).max + 1

    val resultArray =
      Array.ofDim[String](height, width).map(x => x.map(y => ""))
    calculated.par.foreach {
      case (k, v) =>
        val row = indexes(k(0))
        val col = k.drop(1).toInt
        resultArray(row)(col) = v.toString
    }
    resultArray.map(x => x.mkString(",")).mkString("\n")
  }

  def createExprStorage(body: String): Map[String, Expr] =
    body
      .replace("\r", "")
      .split("\n")
      .zipWithIndex
      .par
      .flatMap {
        case (line, row) =>
          val exprStrings = line.split(",").zipWithIndex
          exprStrings.map {
            case (exprStr, col) =>
              val colName = alphabet(row).toString
              val expr = parseExpression(exprStr)
              val cellName = colName + col
              (cellName, expr)
          }
      }
      .toList
      .toMap
  case class CyclicRefException(chain: List[String], cell: String)
      extends RuntimeException
  def calSpreadSheet(exprStorage: Map[String, Expr]) = {

    def eval(expr: Expr, chain: List[String]): Trampoline[Double] =
      expr match {
        case Ref(variable) =>
          if (chain.contains(variable))
            throw CyclicRefException(chain, variable)
          eval(exprStorage(variable), variable :: chain)
        case Add(a, b) =>
          for { e1 <- eval(a, chain); e2 <- eval(b, chain) } yield e1 + e2
        case Sub(a, b) =>
          for { e1 <- eval(a, chain); e2 <- eval(b, chain) } yield e1 - e2
        case Multiply(a, b) =>
          for { e1 <- eval(a, chain); e2 <- eval(b, chain) } yield e1 * e2
        case Divide(a, b) =>
          for { e1 <- eval(a, chain); e2 <- eval(b, chain) } yield e1 / e2
        case Literal(v) => Done(v)
        case _          => throw new RuntimeException("wrong operation")
      }
    exprStorage.par.map {
      case (k, v) =>
        (k, eval(v, List.empty[String]))
    }
  }

  def parseExpression(exprStr: String): Expr = {
    if (exprStr(0) != '=') {
      Literal(exprStr.toInt.toDouble)
    } else {
      val exprTree = MathExpCompiler(exprStr.drop(1))
      exprTree match {
        case Left(_)      => throw new RuntimeException(s"bad expression $exprStr")
        case Right(right) => right
      }
    }
  }
}
