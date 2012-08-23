package ga_mock

import genetic_algorithm._

class MockGALogic(pop: Int, mut: Double, glen: Int) extends GALogic(pop, mut, glen){

	def initGeneration(pop: Int, len: Int): Array[Genom] = {
		generation = new Array[Genom](population)
		for(i <- 0 to generation.length - 1){
		 	var gene = new Array[Int](len)
			gene.map{n => rand.nextInt(10) - 5}
			generation(i) = new MockGenom(gene)
		}
		generation
	}
  
	protected def createIndividual(father: Genom, mother: Genom): Genom = {
		//1点交叉
		val pt = rand.nextInt(father.getLength)
		var genes = new Array[Int](father.getLength())
		
		for(i <- 0 to genes.length - 1){
			var v = 0
			if(i < pt){
				v = father.getGene(i) 
			} else {
				v = mother.getGene(i)
			}
			
			//突然変異
			if(rand.nextDouble() < mutationRate){
				v += rand.nextInt(10) - 5
			}
			genes(i) = v
		}
		new MockGenom(genes)
	}
	
}