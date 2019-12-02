package pl.pgizka.csvParser
import play.api.libs.json.Json
import Node._

import scala.io.Source
import scala.util.Try

object Main {
  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      println("Usage: sbt \"run pathToCsvFile\"")
    } else {
      Try {
        val source = Source.fromFile(args.head)
        FileParser.parseFile(source.getLines()).fold(
          error => println(error),
          nodes => println(Json.toJson(nodes))
        )
        source.close()
      }.fold(
        error => println(s"Error occurred during parsing file $error}"),
        _ => println("Parsed successfully")
      )
    }
  }
}


