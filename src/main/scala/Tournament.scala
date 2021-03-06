package com.github.jedesah

import com.github.nscala_time.time.Imports._
import com.github.jedesah
import util.matching.Regex.Match

/** Representation of a simple elimination tournament */
object Tournament {
  


  /** Represents the location of a Match, for a squash tournament, this would be a court.
      For a soccer tournament this would be the playing field. */
  type MatchLocation = String
  type TimeSlot = (LocalTime, LocalTime)
  type Day = Int
  type Availability = Map[Day, TimeSlot]
  type Availabilities = Map[MatchLocation, Availability]
  type Participant = String

  case class Rules(minTimeBetweenMatch: Duration, expectedMatch:Duration)

  case class Constraints(rules: Rules, availabilities: Availabilities) {
    /** Determines whether the startTime, location and day is valid given these constraints.
	Careful, if the MatchLocation is available from 9 am. to 5 pm. and a match is scheduled for 5 pm.
	Then the match's scheduling would be invalid since the MatchLocation is longer available at the start of the match.
    */
    def isMatchStartTimeValid(startTime: LocalTime, location: MatchLocation, day: Day):Boolean = {
    val time :TimeSlot = availabilities.get(location).get.get(day).get
    val timeDebut: Int = time._1.getHourOfDay*60 + time._1.getMinuteOfHour
    val timeFin :Int = time._2.getHourOfDay*60 + time._2.getMinuteOfHour - rules.expectedMatch.getStandardMinutes.toInt
    val timeMatch: Int = startTime.getHourOfDay*60 + startTime.getMinuteOfHour

      timeMatch >= timeDebut && timeMatch <= timeFin
    }
  }

  case class Tournament(draw: Match, schedule: Map[Match, (MatchLocation, Day, LocalTime)])

  abstract class Match {
    /** The winner of this Match, None, if the match has not been played yet and has no winner or
	Some(Participant) if the match has been played and won
    */
    var winner: Option[Participant] = None
    /** Returns all Matches to play. ie. the matches for whom the two participants is already know. */
    def determinedSubMatches: Set[SimpleMatch]
    /** Returns all SimpleMatchs contained directly or indirectly by this Match */
    def leafSubMatches: Set[SimpleMatch]
    /** Returns all matches that belong to a specified round or None if the round number is invalid. */
    def round(nb: Int): Option[Set[Match]]
    /** Returns all participants who are still contenders for the title. ie they have not lost any of their
    matches as we are modeling a simple elimination tournament.
    */
    def contenders: Set[Participant]
    /** Returns a new Tournament which represents the new state of the tournament resulting form this participants victory */
    def update(winner: Participant): Match
    /** Returns a determined Match involving this participant */
    def findMatchWithParticipant(participant: Participant): Match
    /** Returns all Matches contained directly or indirectly in this Match */
    def allMatches: Set[Match]
    /** Returns the number of rounds in this Match
	Returns 1 if this match is a ByeMatch or a simple Match;
	Returns the depth of the Tree if this Match is a composite Match
    */
    def nbRounds: Int
  }
  case class SimpleMatch(first: Participant, second: Participant) extends Match {
    def this(first: Participant, second: Participant, winner_ : Participant) = {
      this(first, second)
      winner = Some(winner_)
    }
    def determinedSubMatches: Set[SimpleMatch] = Set(this)
    def leafSubMatches: Set[SimpleMatch] = Set()
    def round(nb: Int): Option[Set[Match]] = {
      if(this.nbRounds == nb)
        Option(Set(this))
      else
        Option(Set())
    }
    def contenders: Set[Participant] = Set(this.winner.toString())
    def update(winner: Participant): Match = new SimpleMatch(first, second, winner)
    def findMatchWithParticipant(participant: Participant): Match = {
      var unMatch:Match = ByeMatch("dummy")
      if (first == participant || second == participant)
        unMatch = this
      unMatch
    }
    def allMatches: Set[Match] = Set(this)
    def nbRounds: Int = 1
  }
  /** When a tournament does not have a number of participants that is a power of 2, it is necessary to give some
      participants a bye. A bye means a partcipant does not have to participate in the first round of the tournament,
      they simply play against one of the winners of the first round.
  */
  case class ByeMatch(only: Participant) extends Match {
    def determinedSubMatches: Set[SimpleMatch] = Set()
    def leafSubMatches: Set[SimpleMatch] = Set()
    def round(nb: Int): Option[Set[Match]] = {
      if(this.nbRounds == nb)
        Option(Set(this))
      else
        Option(Set())
    }
    def contenders: Set[Participant] = Set(this.winner.toString())
    def update(winner: Participant): Match = this
    def findMatchWithParticipant(participant: Participant): Match = {
      var unMatch:Match = ByeMatch("dummy")
      if (only == participant)
        unMatch = this
      unMatch
     }
    def allMatches: Set[Match] = Set(this)
    def nbRounds: Int = 1
  }
  /** A CompositeMatch is a Match that opposes the winner of the first match against the winner of the second. */
  case class CompositeMatch(first: Match, second: Match) extends Match {
    def this(first: Match, second: Match, winner_ : Participant) = {
      this(first, second)
      winner = Some(winner_)
    }
    def determinedSubMatches: Set[SimpleMatch] = {
      var matches :Set[SimpleMatch] = Set()
      first.determinedSubMatches.foreach(e => matches += e)
      second.determinedSubMatches.foreach(e=> matches += e)
      matches
    }
    def leafSubMatches: Set[SimpleMatch] = ???
    def round(nb: Int): Option[Set[Match]] = ???
    def contenders: Set[Participant] = ???
    def update(match_ : Match, winner: Participant): Match = {
      var unMatch :Match = ByeMatch("dummy")
      allMatches.foreach(e=>
        if (e == match_)
         unMatch = e.update(winner)
      )
      unMatch
    }
    def update(winner: Participant): Match = new CompositeMatch(first, second, winner)
    def findMatchWithParticipant(participant: Participant): Match = ???
    def allMatches: Set[Match] = ???
    def nbRounds: Int = ???
  }
  
  /** Generate a tournament that satisfies the specified constraints */
  def generate(constraints: Constraints, participants: Set[Participant]): Tournament = {
    if(participants.size <= 1) { throw new IllegalArgumentException("un seul joueur") }
    ???
  }
}