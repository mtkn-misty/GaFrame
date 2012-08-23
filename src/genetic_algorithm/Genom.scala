package genetic_algorithm

import scala.collection.mutable._

abstract class Genom {
	private var fit: Double = 0.0
	/**
	 * 適応度を計算して返す
	 * キャッシュがあればそれを利用する
	 */
	def getFitnessWithCache(): Double = {
		val key = getKey()
		if(Genom.genomCache.get(key) == None){
			fit = getFitness()
			Genom.genomCache.put(getKey(), this)
		}
		fit
	}
	
	/**
	 * 適応度を計算して返す
	 * ここで評価関数を定義する
	 */	
	def getFitness(): Double
	
	/**
	 * i番目の遺伝子を返す
	 */
	def getGene(i: Int): Int
	
	/**
	 * ゲノムの長さを返す
	 */
	def getLength(): Int
	
	/**
	 * キャッシュ用のキー
	 */
	def getKey(): String
	
}

object Genom{
	var genomCache = new HashMap[String, Genom] 
	
	def getCache(): Map[String, Genom] = {
		genomCache
	}
	
	def getRanking(): List[(String, genetic_algorithm.Genom)] = {
		genomCache.toList.sortWith((l, r) => l._2.getFitnessWithCache() > r._2.getFitnessWithCache())
	}
}

