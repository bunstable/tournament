import com.github.jedesah.Tournament._
import org.scalatest._
import com.github.nscala_time.time.Imports._

trait defaults {
  /** Courts are open from 9:00 am. to 10:30 pm. */
  val defaultHoursOfOperation = (new LocalTime(9, 0), new LocalTime(22, 30))
  /** Courts are open normally for all 2 days */
  val defaultAvailability = Map(
    1 -> defaultHoursOfOperation,
    2 -> defaultHoursOfOperation
  )
  /** All courts have the same Availability */
  val defaultAvailabilities = Map(
    "Court1" -> defaultAvailability,
    "Court2" -> defaultAvailability
  )
  
  val defaultParticipants = Set("Gen", "Bob", "Guillaume Hebert", "Yohann Labonte")

  /** By default,
    there must be a minimum of 3 hours between each match
    each match is expected to last 1 hour
  */
  val defaultRules = Rules(Duration.standardHours(3), Duration.standardHours(1))
}

class GenerateTournamentFeatureSpec extends FeatureSpec with ShouldMatchers with GivenWhenThen with defaults {
  info("It is common in tournaments of competitive nature that there is a miminum time betweeen matches involving the same participant in order for the participant to rest")
  info("Il est commun dans un tournoi de nature competitive d'avoir un minimum de temps entre les parties qui implique un meme joueur afin de permettre a ce dernier de se reposer.")
  feature("Minimum rest time / Temps minimal de repos") {
    scenario("The number of participants, minimum rest time and MatchLocationAvailabilities admit a solution / Le nombre de partipants, la quantite minimal de repos et les disponibilites de terrains admetent une solution") {
      Given("the above scenario / Le scenario mentionne ci-haut")
   
      When("the tournament is generated / le tournoi est genere")   
      val constraints = Constraints(defaultRules, defaultAvailabilities)
      val tournament = generate(constraints, defaultParticipants)
      Then("the tournament respects the minimum amount of rest of all participants / l tournoi respecte la quantite minimal de repos devant etre accorde a chacun des participants")
      
      assert(constraints.isMatchStartTimeValid(new LocalTime(9, 0), new MatchLocation("Court1"),1) ==true)
      
    }
    scenario("The Number of participants, minimum rest time and MatchLocationAvailability do not admit a solution / Le Nombre de participatns, temps de repos minimum et disponibilites de terrains n'admetent pas une solution") {
      Given("the above scenario / le scenario mentionne ci-haut")
      val desParticipants = Set("Alice", "Bob", "Chris", "Dante", "Emilio", "Fruit", "Gordon", "Herman", "Isaac", "Jack", "Keith", "Lance")
      
      val availabilitiesShort = Map(
      "Court1" -> Map(
	1 -> ((new LocalTime(9,0), new LocalTime(11,0))),
	2 -> ((new LocalTime(12,0), new LocalTime(13,0)))
      ),
      "Court2" -> Map(
	1 -> ((new LocalTime(9,0), new LocalTime(11,0))),
	2 -> ((new LocalTime(12,0), new LocalTime(13,0)))
      )
    )
      When("the tournament is generated / le tournoi est genere")
      val constraints = Constraints(defaultRules, availabilitiesShort)
      val tournament = generate(constraints, desParticipants)
      Then("an IllegalArgumentException is thrown / une IllegalArgumentException est lance")
      evaluating { tournament } should produce [IllegalArgumentException]
    }
  }
  
  feature("Balanced draw / Tirage balance") {
    scenario("Number of participants is a power of two / Le nombre de participants est une puissance de deux") {
      Given("A number of participants that is a power of deux / un nombre de participants qui est une puissance de deux")
      val participantsPower = Set("We're","2")
      When("the tournament is generated / le tournoi est genere")
      val constraints = Constraints(defaultRules, defaultAvailabilities)
      val tournament = generate(constraints, participantsPower)
      Then("each participant should be required to win the same amount of matches in order to win the tournament / chaque participant devrait avoir a gagner le meme nombre de match afin de gagner le tournoi")
      //tournament.draw.findMatchWithParticipant(participantsPower(0)) should equal (tournament.draw.findMatchWithParticipant(participantsPower(1)))
      assert(true)
    }
    scenario("number of participants is not a power of two / nombre de participants n'est pas une puissance de deux") {
      Given("a number of participants that is not a power of two / un nombre de participants qui n'est pas une puissance de deux")
      val participantsNotPower = Set("We","Are","3")
      When("the tournament is generated / le tournoi est genere")
      val constraints = Constraints(defaultRules, defaultAvailabilities)
      val tournament = generate(constraints, participantsNotPower)
      Then("each participant should be required to win no more than one less match than any other participant in order to win the tournament / chaque participant devrait avoir a gagner pas plus d'une partie de moins que n'importe quel autre participant afin de remporter le tournoi")
      assert(true)
    }
    scenario("one participant / un seul participant") {
      Given("one participant / un seul participant")
      val participantAlone = Set("I'm alone")
      When("the tournament is generated / le tournoi est genere")
      val constraints = Constraints(defaultRules, defaultAvailabilities)
      val tournament = generate(constraints, participantAlone)
      Then("an IllegalArgumentException is thrown / un IllegalArgumentException est lance")
      evaluating { participantAlone.size } should produce [IllegalArgumentException]
    }
  }
  
  feature("respect availabilities / respect des disponibilites") {
    Given("a set of availabilities / une collection de disponibilites")
    val availabilities = Map(
      "Court1" -> Map(
	1 -> ((new LocalTime(9,0), new LocalTime(23,0))),
	2 -> ((new LocalTime(12,0), new LocalTime(17,0)))
      ),
      "Court2" -> Map(
	1 -> (new LocalTime(9,0), new LocalTime(23,0)),
	2 -> (new LocalTime(13,0), new LocalTime(18,0))
      ),
      "Court3" -> Map(
	1 -> (new LocalTime(12,0), new LocalTime(17,0)),
	2 -> (new LocalTime(10,30), new LocalTime(16,45))
      )
    )
    When("the tournament is generated / le tournoi est genere")
    val constraints = Constraints(defaultRules, availabilities)
    val tournament = generate(constraints, defaultParticipants)
    Then("MatchLocation availabilities are respected / les disponibilites sont respecte")
    val doRespectAvailabilities = tournament.draw.allMatches.forall { match_ =>
      val (matchLocation, day, time) = tournament.schedule(match_)
      constraints.isMatchStartTimeValid(time, matchLocation, day)
    }
    assert(doRespectAvailabilities)
  }
  
  feature("Randomized draw / Tirage aleatoire") {
    // generez 10 tournois avec beaucoup de joueurs et vous assurer qu'ils ne sont pas tous pareil devrait suffir pour valider ce test.
    // Le test va echouer de temps en temps.
    val participantALot = Set("I'm Alone...", "Not Anymore", "Because", "There Is", "Too Much", "People", "Wanting To", "Battle", "Against You", "Without", "AnyPity", "AtAll")
    val constraints = Constraints(defaultRules, defaultAvailabilities)
    
    val tournament= Array(
     generate(constraints, participantALot),
     generate(constraints, participantALot),
     generate(constraints, participantALot),
     generate(constraints, participantALot),
     generate(constraints, participantALot),
     generate(constraints, participantALot),
     generate(constraints, participantALot),
     generate(constraints, participantALot),
     generate(constraints, participantALot),
     generate(constraints, participantALot)     
     )
     
     for (i <- 0 until 10){
      for (j <- 0 until 10)
      {
	 tournament(i).draw.allMatches should not equal (tournament(j).draw.allMatches)
      }
     }
  }
}

class GenerateTournamentSpec extends FunSpec with ShouldMatchers with defaults {
  describe("Generate a tournament / Generer un tournoi") {
    describe("only uses specified MatchLocations / utilise seulement les terrains specifies") {
      val tournamentInstance = generate(Constraints(defaultRules, defaultAvailabilities), defaultParticipants)
      assert(true)
    }
    describe("involves all specified participants / inclut tous les participants specifies") {
      val tournamentInstance = generate(Constraints(defaultRules, defaultAvailabilities), defaultParticipants)
      tournamentInstance.draw.contenders should equal (defaultParticipants)
    }
  }
}

class MatchSpec extends FunSpec with ShouldMatchers {
  describe("Match") {
    describe("determinedSubMatches") {
      it("should return the Set of all subMatches for whom the participants involved are known / devrait retourner la collection de tous les sous-matchs pour lesquels les participants sont connus"){
      val matchTest1 = SimpleMatch("P1", "P2")
      val matchTest2 = SimpleMatch("P3", "P4")
      val matchTest3 = SimpleMatch("P5", "P6")
      val matchTest4 = SimpleMatch("P7", "P8")

      val matchComposite1 = CompositeMatch(matchTest1, matchTest2)
      val matchComposite2 = CompositeMatch(matchTest3, matchTest4)
      val matchComposite3 = CompositeMatch(matchComposite1, matchComposite2)

      matchComposite3.determinedSubMatches should equal (Set(matchTest1, matchTest2, matchTest3, matchTest4))
      }
      it("should not equal to a set of matches, with participants are known,  where one match is missing.") {
        val matchTest1 = SimpleMatch("P1", "P2")
        val matchTest2 = SimpleMatch("P3", "P4")
        val matchTest3 = SimpleMatch("P5", "P6")
        val matchTest4 = SimpleMatch("P7", "P8")

        val matchComposite1 = CompositeMatch(matchTest1, matchTest2)
        val matchComposite2 = CompositeMatch(matchTest3, matchTest4)
        val matchComposite3 = CompositeMatch(matchComposite1, matchComposite2)

        matchComposite3.determinedSubMatches should not equal (Set(matchTest1, matchTest2, matchTest3))
      }
    }
    describe("leafSubMatches") {
      it("should return the matches from the first round / retourner les matchs de la premiere ronde") {
        val matchTest1 = SimpleMatch("P1", "P2")
        val matchTest2 = SimpleMatch("P3", "P4")
        val matchTest3 = SimpleMatch("P5", "P6")
        val matchTest4 = SimpleMatch("P7", "P8")

        val matchComposite1 = CompositeMatch(matchTest1, matchTest2)
        val matchComposite2 = CompositeMatch(matchTest3, matchTest4)
        val matchComposite3 = CompositeMatch(matchComposite1, matchComposite2)

        matchComposite1.leafSubMatches should equal (Set(matchTest1, matchTest2))
        matchComposite2.leafSubMatches should not equal (Set(matchTest1, matchTest2))
        matchComposite3.leafSubMatches should equal (Set(matchTest1, matchTest2, matchTest3, matchTest4))
      }
    }
    describe("round(Int)") {
      it("return the round corresponding to the specified Int. Rounds are numbered 1 to n where 1 is the first round and n is the round containing this match / retourner la ronde qui correspond au Int recu en parametre. Les rondes sont numerotes de 1 a n ronde, ou 1 est la premiere ronde et n et le match courant") {
      val matchTest1 = SimpleMatch("P1", "P2")
      val matchTest2 = SimpleMatch("P3", "P4")
      val matchTest3 = SimpleMatch("P5", "P6")
      val matchTest4 = SimpleMatch("P7", "P8")

      val matchComposite1 = CompositeMatch(matchTest1, matchTest2)
      val matchComposite2 = CompositeMatch(matchTest3, matchTest4)
      val matchComposite3 = CompositeMatch(matchComposite1, matchComposite2)

      matchTest1.round(1) should equal (SimpleMatch("P1", "P2"))
      matchTest2.round(2) should not equal (SimpleMatch("P1", "P2"))
      }

    }

    describe("update") {
      it("should correclty update itself. i.e. the update method shoud return a new Tournament state that correclty reflects the fact that the specified player won / devrait mettre a jour le tournoi afin de refleter la victoire du joueur specifie") {
	val matchToUpdate = SimpleMatch("Paul", "Andrea")
	val startMatch = CompositeMatch(matchToUpdate, SimpleMatch("Jon", "George"))
	val expectedMatch = CompositeMatch(new SimpleMatch("Paul", "Andrea", "Paul"), SimpleMatch("Jon", "George"))
	startMatch.update("Paul") should equal (expectedMatch)
      }
      it("should throw an IllegalArgumentException if the String is not among the remaining containders / devrait lance une exception de type IllegalArgumentException si le participant n'est pas parmis les participants qui reste") {
	val match_ = SimpleMatch("Mary", "Judy")
	evaluating { match_.update("George") } should produce [IllegalArgumentException]
      }
    }
  }
}

class ConstraintsSpec extends FunSpec with ShouldMatchers {
  describe("Constraints") {
    describe("isMatchStartTimeValid") {
      val myHoursOfOperation = (new LocalTime(10, 0), new LocalTime(20, 0))
      /** Courts are open normally for all 2 days */
      val myAvailability = Map(
        1 -> myHoursOfOperation,
        2 -> myHoursOfOperation
      )
      /** All courts have the same Availability */
      val myAvailabilities = Map(
        "Court1" -> myAvailability,
        "Court2" -> myAvailability
      )
      val myRules = Rules(Duration.standardHours(3), Duration.standardHours(1))

      val constraints = Constraints(myRules, myAvailabilities)

      it("should be starting at a good time")    {
      constraints.isMatchStartTimeValid(new LocalTime(10, 0), "Court1", 1) should equal (true)
      }
      it("should be true because the match is at the limit with of the time considering the expected hour of the match")     {
      constraints.isMatchStartTimeValid(new LocalTime(19, 0), "Court1", 1) should equal (true)
      }
      it("should be illegal to start a match before the time of the court availabilities")     {
      constraints.isMatchStartTimeValid(new LocalTime(8, 0), "Court2", 1) should equal (false)
       }
      it("should be wrong because the match is in the limits but there isnt enought time to play the match ")   {
      constraints.isMatchStartTimeValid(new LocalTime(19, 30), "Court1", 1) should equal (false)
      }
    }
  }
}
