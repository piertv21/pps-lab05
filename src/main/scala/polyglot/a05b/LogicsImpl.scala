package polyglot.a05b

import polyglot.a05b.Logics

import scala.util.Random

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a05b/sol2/ */
class LogicsImpl(private val size: Int) extends Logics:

  private case class Pair[A, B](x: A, y: B)

  private val gridSize: Int = size
  private val initial: Pair[Int, Int] = new Pair(Random.nextInt(gridSize - 2) + 1, Random.nextInt(gridSize - 2) + 1)
  private var tickCount: Int = 0

  override def tick(): Unit = {
    tickCount += 1
  }

  override def hasElement(x: Int, y: Int): Boolean = {
    val dx = Math.abs(x - initial.x)
    val dy = Math.abs(y - initial.y)
    dx <= tickCount || dy <= tickCount ||
      (x - y == initial.x - initial.y && dx <= tickCount) ||
      (x + y == initial.x + initial.y && dx <= tickCount)
  }

  override def isOver: Boolean = {
    initial.y - tickCount < 0 || initial.y + tickCount >= gridSize ||
      initial.x - tickCount < 0 || initial.x + tickCount >= gridSize
  }