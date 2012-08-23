package ga_mock

object MockGAMain {
	def main(args: Array[String]): Unit = {
		var mockGA = new MockGALogic(10, 0.1, 5)
		
		(0 to 10).foreach{i =>
			mockGA.step()
			println("#> " + i +  " - " + mockGA.getAveFitness())
		}
		mockGA.getRanking().foreach{v => 
		  	println(v._2)
		}
	}
}