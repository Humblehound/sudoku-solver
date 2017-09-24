import scala.collection.immutable
import scala.io.Source

object Solver {

  def main(args: Array[String]) {
    val readmeText = Source.fromResource("diff8").getLines()
    val board = Array.ofDim[String](9)
    readmeText.copyToArray(board)
    val sudoku = board.map {
      p =>
        p.map {
          case '-' => FieldSolver()
          case x => FieldSolver(x.toInt - 48)
        }
    }.map(_.toArray)

    solve(sudoku)
  }

  private def removeSameBox(boxes: IndexedSeq[Box]) = boxes.map {
    box => {
      val presentNumbers = box.values.flatMap(_.map(x => x.value).toSet).toSet.filterNot(i => i == 0)
      box.copy(values = box.values.map(_.map(value => {
        value.copy(potential = value.potential filterNot presentNumbers.contains)
      })))
    }
  }


  private def removeColumn(boxes: IndexedSeq[Box]) = {
    val omg = boxes.groupBy(_.column)
    val lol = omg.map{
      case (_, boxes) =>
        for{
          i <- 0 to 2
        } yield{
          val colValues = boxes.flatMap {
            box => box.values.map(row => row(i).value)
          }.toSet.filterNot(x => x == 0)
          boxes.map {
            box => {
              box.copy(values = box.values.map {
                x => x.updated(i,  x(i).copy(potential = x(i).potential filterNot colValues.contains))
              })
            }
          }
        }
    }
    lol
  }

  def removeRow(boxes: IndexedSeq[Box]) = {

  }

  def solve(sudoku: Array[Array[FieldSolver]]): Unit = {

    val boxes = for (
      i <- 0 to 2;
      j <- 0 to 2
    ) yield Box(i, j, Array())

    val boxValues = boxes.map {
      box => {
        val boxValues = sudoku.zipWithIndex.collect {
          case (x, i) if i >= (box.row * 3) && i < (box.row * 3 + 3) => x.zipWithIndex.collect {
            case (y, j) if j >= (box.column * 3) && j < (box.column * 3 + 3) => y
          }
        }
        box.copy(values = boxValues)
      }
    }

    val stage1 = removeSameBox(boxValues)
    val stage2 = removeColumn(stage1)
    //    val stage3 = removeColumn(stage2)
  }
}

case class FieldSolver(value: Int = 0, potential: List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9))

case class Box(row: Int, column: Int, values: Array[Array[FieldSolver]])
