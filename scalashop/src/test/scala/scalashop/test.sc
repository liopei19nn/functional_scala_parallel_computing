object test {
	val numTasks = 5
	val src = 2




	def workLoadBanlancer(start: Int, taskNum: Int, tasksCount: Int, acc: List[List[Int]]): List[List[Int]] = {
		val maxRowsInTask = (src - start + tasksCount - 1) / tasksCount
		val currentRowsInTask: List[Int] = {
			for {
				rowNum <- start until start + maxRowsInTask
				if (src - rowNum >= tasksCount - taskNum)
			} yield rowNum
		} toList

		if(taskNum == numTasks - 1) acc :+ currentRowsInTask
		else workLoadBanlancer(start + currentRowsInTask.length, taskNum + 1, tasksCount - 1, acc :+ currentRowsInTask)
	}


	val workLoad = workLoadBanlancer(0, 0, numTasks, List()).filter(!_.isEmpty)

}