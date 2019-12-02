package pl.pgizka.csvParser

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import pl.pgizka.csvParser.FileParser.parseFile

class FileParserTest extends AnyWordSpec with Matchers {

  "FileParser" can {

    "parse valid file" when {
      "sample file" in {
        val validFile = Seq(
          "Poziom 1,Poziom 2,Poziom 3,ID",
          "A,,,1",
          ",AA,,2",
          ",,AA1,3",
          ",,AA2,4",
          ",AB,,5",
          "B,,,6",
          "C,,,7",
          ",CA,,8",
          ",,CA1,9",
          ",,CA2,10",
          "D,,,11",
          ",DA,,12",
        )

        parseFile(validFile.toIterator) mustBe ('right)
      }

      "file with only two columns" in {
        val validFile = Seq(
          "Poziom 1,ID",
          "A,1",
          "B,2",
          "C,3",
        )

        parseFile(validFile.toIterator) mustBe ('right)
      }

      "file with 6 columns" in {
        val validFile = Seq(
          "Poziom 1,Poziom 2,Poziom 3,Poziom 4,Poziom 5,Poziom 6,ID",
          "A,,,,,,1",
          ",AA,,,,,2",
          ",,AAA,,,,3",
          ",,,AAAA,,,4",
          ",,,,AAAAA,,5",
          ",,,,,AAAAAA,6",
        )

        parseFile(validFile.toIterator) mustBe ('right)
      }
    }

    "parse invalid file" when {
      "file is empty" in {
        val file = Seq(
          "",
        )
        parseFile(file.toIterator) mustBe ('left)
      }

      "file contains only one line" in {
        val file = Seq(
          "Poziom 1,Poziom 2,Poziom 3,ID",
        )
        parseFile(file.toIterator) mustBe ('left)
      }

      "There are less then two columns" in {
        val file = Seq(
          "Poziom 1",
          "A",
        )
        parseFile(file.toIterator) mustBe ('left)
      }

      "one line has incorrect number of columns" in {
        val file = Seq(
          "Poziom 1,Poziom 2,Poziom 3,ID",
          "A,,,1",
          ",AA,,,2",
        )
        parseFile(file.toIterator) mustBe ('left)
      }

      "last cell cannot be parsed to integer" in {
        val file = Seq(
          "Poziom 1,Poziom 2,Poziom 3,ID",
          "A,,,aaa",
        )
        parseFile(file.toIterator) mustBe ('left)
      }

      "node id is duplicated" in {
        val file = Seq(
          "Poziom 1,Poziom 2,Poziom 3,ID",
          "A,,,1",
          ",AA,,1",
        )
        parseFile(file.toIterator) mustBe ('left)
      }

      "node does not have a parent" in {
        val file = Seq(
          "Poziom 1,Poziom 2,Poziom 3,ID",
          "A,,,1",
          ",,AAA,2",
        )
        parseFile(file.toIterator) mustBe ('left)
      }

      "all cells in line are empty" in {
        val file = Seq(
          "Poziom 1,Poziom 2,Poziom 3,ID",
          "A,,,1",
          ",,,2",
        )
        parseFile(file.toIterator) mustBe ('left)
      }

      "more than one cell contains node name" in {
        val file = Seq(
          "Poziom 1,Poziom 2,Poziom 3,ID",
          "A,,,1",
          ",AA,AAA,2",
        )
        parseFile(file.toIterator) mustBe ('left)
      }
    }
  }

}
