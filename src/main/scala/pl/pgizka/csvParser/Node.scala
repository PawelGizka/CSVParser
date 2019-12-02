package pl.pgizka.csvParser

import play.api.libs.json.{Format, Json}

case class Node(id: Int, name: String, nodes: List[Node])

object Node {
  implicit val NodeFormat: Format[Node] = Json.format[Node]
}
