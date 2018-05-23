import scopt.OptionParser

case class Config(inputFile: String = "", outputFile: String = "")
object Opt {
  val parser: OptionParser[Config] =
    new scopt.OptionParser[Config]("spreadsheet") {
      opt[String]('i', "input")
        .required()
        .action((x, c) => c.copy(inputFile = x))
      opt[String]('o', "output")
        .required()
        .action((x, c) => c.copy(outputFile = x))
    }
}
