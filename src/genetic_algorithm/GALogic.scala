package genetic_algorithm

import scala.util.Random
import scala.collection.mutable._

abstract class GALogic(pop: Int, mut: Double, glen: Int) {
	val population: Int = pop
	val mutationRate: Double = mut
	val geneLen: Int = glen
	
	val rand = new Random
  
	var numOfGeneration = 0
	var generation = initGeneration(population, geneLen)
	var aveFit: Double = 0.0

	/**
	 * 初期化
	 */
	def initGeneration(pop: Int, glen: Int): Array[Genom]
	
	
	/**
	 * 交叉して個体を一体生成する
	 */
	protected def createIndividual(father: Genom, mother: Genom): Genom
	
	/**
	 * 現集団から1体個体を選択する
	 */
	protected def selectIndividual(total: Double): Genom = {
		var dart: Double = rand.nextDouble() * total //ルーレットに刺したダーツ
		var sum: Double = 0.0
		var genom: Genom = null
		for(i <- 0 to generation.length) {
			if(genom == null) {
				sum += generation(i).getFitnessWithCache()
				if(sum > dart){
					genom = generation(i)
				}
			}
		}
		genom
	}
	
	/**
	 * 次の世代を生成する
	 */
	protected def createNextGeneration(): Array[Genom] =  {
		/*
		 * ルーレット選択をして、選択された個体から次の世代の個体を生成する
		 */
	  
		//適応度の総計を計算（選択のため）
		//ここで評価たものをキャッシュに入れる
		var total: Double = 0.0
		generation.foreach{genom =>{
			total += genom.getFitnessWithCache() 
		}}
	    aveFit = total / generation.length
	    
	    var nextGeneration = new Array[Genom](population)
		for(i <- 0 to nextGeneration.length - 1){
		  	//親となる個体を2体選択し子を生成する
		  	nextGeneration(i) = createIndividual(selectIndividual(total), selectIndividual(total))
		}
		nextGeneration
	}
	
	def step(): Unit = {
		generation = createNextGeneration()
	}
	
	def getGeneration(): Array[Genom] = {
		generation
	}
	
	def getAveFitness(): Double = {
		aveFit
	}
	  
	def getCache(): Map[String, Genom] = {
		Genom.getCache
	}
	
	def getRanking(): List[(String, genetic_algorithm.Genom)] = {
		Genom.getRanking
	}
}