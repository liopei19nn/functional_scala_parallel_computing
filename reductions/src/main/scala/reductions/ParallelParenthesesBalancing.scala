package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

	@volatile var seqResult = false

	@volatile var parResult = false

	val standardConfig = config(
		Key.exec.minWarmupRuns -> 1,
		Key.exec.maxWarmupRuns -> 1,
		Key.exec.benchRuns -> 120,
		Key.verbose -> true
	) withWarmer (new Warmer.Default)

	def main(args: Array[String]): Unit = {
		val length = 100000000
		val chars = new Array[Char](length)
		val threshold = 10000
		val seqtime = standardConfig measure {
			seqResult = ParallelParenthesesBalancing.balance(chars)
		}
		println(s"sequential result = $seqResult")
		println(s"sequential balancing time: $seqtime ms")

		val fjtime = standardConfig measure {
			parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
		}
		println(s"parallel result = $parResult")
		println(s"parallel balancing time: $fjtime ms")
		println(s"speedup: ${seqtime / fjtime}")
	}
}

object ParallelParenthesesBalancing {

	/** Returns `true` iff the parentheses in the input `chars` are balanced.
		*/
	def balance(chars: Array[Char]): Boolean = {
		def helper(charArr: Array[Char], count: Int): Boolean = {
			if (charArr.isEmpty) {
				count == 0
			} else if (count < 0) return false
			else {
				if (charArr.head == '(') helper(charArr.tail, count + 1)
				else if (charArr.head == ')') helper(charArr.tail, count - 1)
				else helper(charArr.tail, count)
			}
		}

		helper(chars, 0)
	}

	/** Returns `true` iff the parentheses in the input `chars` are balanced.
		*/
	def parBalance(chars: Array[Char], threshold: Int): Boolean = {
		def traverse(idx: Int, until: Int, openCount: Int, closeCount: Int): (Int, Int) = {
			if (idx < until) {
				chars(idx) match {
					case '(' => traverse(idx + 1, until, openCount + 1, closeCount)
					case ')' =>
						if (openCount > 0) traverse(idx + 1, until, openCount - 1, closeCount)
						else traverse(idx + 1, until, openCount, closeCount + 1)
					case _ => traverse(idx + 1, until, openCount, closeCount)
				}
			} else (openCount, closeCount)
		}
		// extension of traverse in essence
		// the key is rightopen and can do nothing to leftclose and rightclose,
		// but leftopen can combine rightclose
		def reduce(from: Int, until: Int): (Int, Int) = {
			if (until - from <= threshold) traverse(from, until, 0, 0)
			else {
				val mid = from + (until - from) / 2
				val ((leftOpenCount, leftCloseCount), (rightOpenCount, rightCloseCount)) =
					parallel(reduce(from, mid), reduce(mid, until))

				if (leftOpenCount > rightCloseCount) ( leftOpenCount + rightOpenCount - rightCloseCount, leftCloseCount)
				else ( rightOpenCount, leftCloseCount + rightCloseCount - leftOpenCount)

			}
		}
		reduce(0, chars.length) ==(0, 0)
	}

	// For those who want more:
	// Prove that your reduction operator is associative!

}
