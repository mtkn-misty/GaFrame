package ga_mock

import scala.math._
import genetic_algorithm._


class MockGenom (g: Array[Int]) extends Genom {
	
	val genes = g
	
	
	/**
	 * 適応度を計算
	 * 適当に4次関数とする
	 */
	def getFitness(): Double = {
		var sum = 0.0
		genes.foreach{sum += _ * 0.1}
		var fit = -sum * sum + 20
		
		if(fit < 0.1){
			fit = 0.1
		}
		fit
	}
	
	def getGene(index: Int): Int = {
		genes(index)
	}
	
	def getLength(): Int = {
		genes.length
	}
	
	def getKey(): String = {
		var key = ""
		genes.foreach{
		    key += _ + '-'
		}
		key
	}
  
	override def toString(): String = {
		"(" + genes.mkString(", ") + ") => " + getFitnessWithCache()
	}
}