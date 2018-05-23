import java.io.{File, PrintWriter}

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    try {
      import SpreadSheet._
      import Opt._

      val config = Opt.parser.parse(args, Config()).get
      val input =
        Source.fromFile(new File(config.inputFile)).mkString

      val body = input
      val tree = createExprStorage(body)
      val ret =
        SpreadSheet.calSpreadSheet(tree).mapValues(_.run).toList.toMap
      val csv = toCsv(ret)
      val writer = new PrintWriter(new File(config.outputFile))

      writer.write(csv)
      writer.close()

    } catch {
      case e: SpreadSheet.CyclicRefException =>
        print(s"Cyclic reference chain ${e.chain} cell ${e.cell}.")
        System.exit(-1)
    }
  }
}
