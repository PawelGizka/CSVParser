package pl.pgizka.csvParser

import scala.util.Try

object FileParser {
  val MINIMUM_FILE_WIDTH = 2

  case class IntermediateNode(id: Int, name: String)

  sealed trait ParsingState
  case class ParsingError(message: String) extends ParsingState
  case object FirstLine extends ParsingState
  case class InProgress(
    rootNodes: List[IntermediateNode],
    parentToNodes: Map[Int, List[IntermediateNode]],
    cellIndexToParentId: Map[Int, Int],
    lastParentCellIndex: Int,
    nodeIds: Set[Int],
    width: Int,
    lineNumber: Int,
  ) extends ParsingState

  def parseFile(lines: Iterator[String]): Either[String, List[Node]] = {
    val result = lines.foldLeft[ParsingState](FirstLine)((state, line) => state match {
      case error: ParsingError => error
      case FirstLine => parseFirstLine(line)
      case inProgress: InProgress => parseInProgress(inProgress, line)
    })
    result match {
      case error: ParsingError => Left(error.message)
      case FirstLine => Left("File is empty")
      case InProgress(rootNodes, parentToNodes, _, _, _, _, 2) => Left("File contains only one line")
      case InProgress(rootNodes, parentToNodes, _, _, _, _, _) =>
        Right(rootNodes
          .reverse
          .map(intermediateNode => buildNode(intermediateNode, parentToNodes)))
    }
  }

  def parseFirstLine(line: String): ParsingState = {
    val width = line.split(",").length
    val isWidthCorrect = width >= MINIMUM_FILE_WIDTH
    if (isWidthCorrect)
      InProgress(
        rootNodes = List.empty,
        parentToNodes = Map.empty,
        cellIndexToParentId = Map.empty,
        lastParentCellIndex = 0,
        nodeIds = Set.empty,
        width = width,
        lineNumber = 2,
      )
    else
      ParsingError("At least two columns are required")
  }

  def parseInProgress(state: InProgress, line: String): ParsingState = {
    val cells = line.split(",")
    val idOption = cells.lastOption.flatMap(cell => Try(cell.toInt).toOption)
    if (cells.length != state.width) {
      makeError(state, s"File width is ${state.width}, but line has ${cells.length} cells")
    } else if (idOption.isEmpty) {
      makeError(state, "Last cell cannot be parsed to integer")
    } else if(state.nodeIds.contains(idOption.get)) {
      makeError(state, s"Duplicate node id: ${idOption.get}")
    } else {
      getNodeNameAndCellIndex(cells.init.toSeq).fold(
        error => makeError(state, error),
        result => {
          val id = idOption.get
          val (name, index) = result
          val node = IntermediateNode(id, name)
          val isRootNode = index == 0
          if (isRootNode) {
            state.copy(
              rootNodes = (node :: state.rootNodes),
              cellIndexToParentId = Map(0 -> id),
              lastParentCellIndex = 0,
              nodeIds = (state.nodeIds + id),
              lineNumber = state.lineNumber + 1,
            )
          } else if(state.lastParentCellIndex < index - 1) {
            makeError(state, s"Node with name: ${name}, does not have parent")
          } else {
            val parentId = state.cellIndexToParentId(index - 1)
            val siblings = state.parentToNodes.getOrElse(parentId, List.empty)
            state.copy(
              parentToNodes = state.parentToNodes + (parentId -> (node :: siblings)),
              cellIndexToParentId = state.cellIndexToParentId + (index -> id),
              lastParentCellIndex = index,
              nodeIds = (state.nodeIds + id),
              lineNumber = state.lineNumber + 1,
            )
          }
        }
      )
    }
  }

  def getNodeNameAndCellIndex(cells: Seq[String]): Either[String, (String, Int)] = {
    val nonEmptyCells = cells.zipWithIndex.filter{case (line, _) => line.nonEmpty}
    if (nonEmptyCells.isEmpty) {
      Left("All cells are empty")
    } else if (nonEmptyCells.length > 1) {
      Left("More than one cell contains node name")
    } else {
      Right(nonEmptyCells.head)
    }
  }

  def makeError(state: InProgress, message: String): ParsingError =
    ParsingError(s"Line ${state.lineNumber}: $message")

  def buildNode(intermediateNode: IntermediateNode, parentToNodes: Map[Int, List[IntermediateNode]]): Node = {
    val children = parentToNodes.getOrElse(intermediateNode.id, List.empty)
      .reverse
      .map(intermediateNode => buildNode(intermediateNode, parentToNodes))
    Node(intermediateNode.id, intermediateNode.name, children)
  }

}
