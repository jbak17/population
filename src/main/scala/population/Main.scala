package population

/**
  * Created by jeva on 13/06/17.
  */

  import scala.util.Random.nextDouble

  trait Human {
    val age: Int
    val gender: Char
  }

  case class subject(age: Int, gender: Char) extends Human{}

  object Main {

    /*
        SETUP CONDITIONS
     */
    val iterations: Int = 10
    val initial_population: Int = 1400 //in thousands
    val births_per_woman: Double = 7.2
    val year_first_birth: Int = 17
    val year_last_birth: Int = 40
    val life_expectancy: Double = 60.0
    val chance_of_early_death: Double = 0.03

    val chance_of_birth: Double = {
      val yearsActive = year_last_birth - year_first_birth
      births_per_woman / yearsActive
    }

    /*
    Create initial population of size pop. Used at commencement of simulation.
    */
    def buildPopulation(pop: Int): List[Human] = List.fill(pop)(createPerson())

    /*
    Used to create initial people in the population.
    Age is randomly, evenly distributed from 0 to life_expectancy
    Gender is 50/50 M/F
    */
    def createPerson(): Human = {
      val age: Int = scala.util.Random.nextInt(60)
      val gender: Char = if (scala.util.Random.nextDouble() < 0.5) 'M' else 'F'
      subject(age, gender)
    }

    def createBaby(): Human = {
      val gender: Char = if (nextDouble < 0.5) 'F' else 'M'
      subject(0, gender)
    }

    /*
    Checks to see if a woman procreates during this iteration
    All new subjects are aged zero and are 50/50 M/F
     */
    def procreate(woman: Human): Boolean = if (nextDouble < chance_of_birth) true else false

    /*
    Add one year to the age of the subject
     */
    def age(young: Human): Human = subject(young.age + 1, young.gender)

    /*
    At each iteration (year) everyone can die.
    The survivors can procreate.
    New population is returned.
     */
    def iterate(population: List[Human]): List[Human] = {
      //age population
      val olderPopulation: List[Human] = population.map(person => age(person))
      //See who survives
      val survivors: List[Human] = olderPopulation.filter(survives)

      println((population.size - survivors.size) +" died.")

      //new babies, how many people procreate
      val births: Int = survivors.filter(_.gender == 'F').filter(procreate).length

      println(births + " babies were born.")
      //make babies
      val children: List[Human] = List.fill(births)(createBaby())
      val new_population: List[Human] = survivors ::: children

      println("Current population size: " + new_population.size)

      new_population
    }

    def survives(person: Human): Boolean = if ((person.age < life_expectancy && nextDouble() > chance_of_early_death) || (person.age > life_expectancy && nextDouble() > 0.5)) true else false

    def main(args: Array[String]) {
      var society: List[Human] = buildPopulation(initial_population)
      var population_size: List[Int] = List(society.size)

      for (i <- 0 to iterations){
        society = iterate(society)
        population_size = society.size :: population_size

      }
      print(population_size.reverse)
    }
  }
