package scalashop

import org.scalameter._
import common._

object VerticalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }

}

/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur {

  /** Blurs the columns of the source image `src` into the destination image
   *  `dst`, starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each column, `blur` traverses the pixels by going from top to
   *  bottom.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    // TODO implement this method using the `boxBlurKernel` method
    for (
      x <- from to end if x >= 0 && x < src.width;
      y <- 0 until src.height
    ) dst.update(x, y, boxBlurKernel(src, x, y, radius))
  }

  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  columns.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    // TODO implement using the `task` construct and the `blur` method
    def workLoadBanlancer(start: Int, taskNum: Int, tasksCount: Int, acc: List[List[Int]]): List[List[Int]] = {
      val maxColsInTask = (src.width - start + tasksCount - 1) / tasksCount
      val currentColsInTask: List[Int] = {
        for {
          colNum <- start until start + maxColsInTask
          if (src.width - colNum >= tasksCount - taskNum)
        } yield colNum
      } toList

      if(taskNum == numTasks - 1) acc :+ currentColsInTask
      else workLoadBanlancer(start + currentColsInTask.length, taskNum + 1, tasksCount - 1, acc :+ currentColsInTask)
    }

    val workLoad = workLoadBanlancer(0, 0, numTasks, List()).filter(!_.isEmpty)

    val tasks = workLoad map (
        w => {
          task {
            blur(src, dst, w.head, w.head + w.length - 1, radius)
          }
        }
      )

    tasks map (t => t.join())


  }

}
