import jdk.management.jfr.RecordingInfo

import scala.collection.immutable.List
import scala.util.Random

class Carte{
  enum NumeroCarte :
    case un
    case deux
    case trois
    case quatre
    case cinq
    case six
    case sept
    case huit
    case neuf
    case dix
    case vallet
    case dame
    case roi
    case as

  enum Couleur :
    case rouge
    case noir

  enum Type :
    case carreau
    case coeur
    case trefle
    case pique

  enum PlayingCard :
    case P(numero : NumeroCarte, color : Couleur, typ : Type)

  def valueOf(s : String): PlayingCard = s match
    case "as carreau" => PlayingCard.P(NumeroCarte.as, Couleur.rouge, Type.carreau)
    case "deux carreau" => PlayingCard.P(NumeroCarte.deux, Couleur.rouge, Type.carreau)
    case "trois carreau" => PlayingCard.P(NumeroCarte.trois, Couleur.rouge, Type.carreau)
    case "quatre carreau" => PlayingCard.P(NumeroCarte.quatre, Couleur.rouge, Type.carreau)
    case "cinq carreau" => PlayingCard.P(NumeroCarte.cinq, Couleur.rouge, Type.carreau)
    case "six carreau" => PlayingCard.P(NumeroCarte.six, Couleur.rouge, Type.carreau)
    case "sept carreau" => PlayingCard.P(NumeroCarte.sept, Couleur.rouge, Type.carreau)
    case "huit carreau" => PlayingCard.P(NumeroCarte.huit, Couleur.rouge, Type.carreau)
    case "neuf carreau" => PlayingCard.P(NumeroCarte.neuf, Couleur.rouge, Type.carreau)
    case "dix carreau" => PlayingCard.P(NumeroCarte.dix, Couleur.rouge, Type.carreau)
    case "vallet carreau" => PlayingCard.P(NumeroCarte.vallet, Couleur.rouge, Type.carreau)
    case "dame carreau" => PlayingCard.P(NumeroCarte.dame, Couleur.rouge, Type.carreau)
    case "roi carreau" => PlayingCard.P(NumeroCarte.roi, Couleur.rouge, Type.carreau)

    case "as coeur" => PlayingCard.P(NumeroCarte.as, Couleur.rouge, Type.coeur)
    case "deux coeur" => PlayingCard.P(NumeroCarte.deux, Couleur.rouge, Type.coeur)
    case "trois coeur" => PlayingCard.P(NumeroCarte.trois, Couleur.rouge, Type.coeur)
    case "quatre coeur" => PlayingCard.P(NumeroCarte.quatre, Couleur.rouge, Type.coeur)
    case "cinq coeur" => PlayingCard.P(NumeroCarte.cinq, Couleur.rouge, Type.coeur)
    case "six coeur" => PlayingCard.P(NumeroCarte.six, Couleur.rouge, Type.coeur)
    case "sept coeur" => PlayingCard.P(NumeroCarte.sept, Couleur.rouge, Type.coeur)
    case "huit coeur" => PlayingCard.P(NumeroCarte.huit, Couleur.rouge, Type.coeur)
    case "neuf coeur" => PlayingCard.P(NumeroCarte.neuf, Couleur.rouge, Type.coeur)
    case "dix coeur" => PlayingCard.P(NumeroCarte.dix, Couleur.rouge, Type.coeur)
    case "vallet coeur" => PlayingCard.P(NumeroCarte.vallet, Couleur.rouge, Type.coeur)
    case "dame coeur" => PlayingCard.P(NumeroCarte.dame, Couleur.rouge, Type.coeur)
    case "roi coeur" => PlayingCard.P(NumeroCarte.roi, Couleur.rouge, Type.coeur)

    case "as trefle" => PlayingCard.P(NumeroCarte.as, Couleur.noir, Type.trefle)
    case "deux trefle" => PlayingCard.P(NumeroCarte.deux, Couleur.noir, Type.trefle)
    case "trois trefle" => PlayingCard.P(NumeroCarte.trois, Couleur.noir, Type.trefle)
    case "quatre trefle" => PlayingCard.P(NumeroCarte.quatre, Couleur.noir, Type.trefle)
    case "cinq trefle" => PlayingCard.P(NumeroCarte.cinq, Couleur.noir, Type.trefle)
    case "six trefle" => PlayingCard.P(NumeroCarte.six, Couleur.noir, Type.trefle)
    case "sept trefle" => PlayingCard.P(NumeroCarte.sept, Couleur.noir, Type.trefle)
    case "huit trefle" => PlayingCard.P(NumeroCarte.huit, Couleur.noir, Type.trefle)
    case "neuf trefle" => PlayingCard.P(NumeroCarte.neuf, Couleur.noir, Type.trefle)
    case "dix trefle" => PlayingCard.P(NumeroCarte.dix, Couleur.noir, Type.trefle)
    case "vallet trefle" => PlayingCard.P(NumeroCarte.vallet, Couleur.noir, Type.trefle)
    case "dame trefle" => PlayingCard.P(NumeroCarte.dame, Couleur.noir, Type.trefle)
    case "roi trefle" => PlayingCard.P(NumeroCarte.roi, Couleur.noir, Type.trefle)

    case "as pique" => PlayingCard.P(NumeroCarte.as, Couleur.noir, Type.pique)
    case "deux pique" => PlayingCard.P(NumeroCarte.deux, Couleur.noir, Type.pique)
    case "trois pique" => PlayingCard.P(NumeroCarte.trois, Couleur.noir, Type.pique)
    case "quatre pique" => PlayingCard.P(NumeroCarte.quatre, Couleur.noir, Type.pique)
    case "cinq pique" => PlayingCard.P(NumeroCarte.cinq, Couleur.noir, Type.pique)
    case "six pique" => PlayingCard.P(NumeroCarte.six, Couleur.noir, Type.pique)
    case "sept pique" => PlayingCard.P(NumeroCarte.sept, Couleur.noir, Type.pique)
    case "huit pique" => PlayingCard.P(NumeroCarte.huit, Couleur.noir, Type.pique)
    case "neuf pique" => PlayingCard.P(NumeroCarte.neuf, Couleur.noir, Type.pique)
    case "dix pique" => PlayingCard.P(NumeroCarte.dix, Couleur.noir, Type.pique)
    case "vallet pique" => PlayingCard.P(NumeroCarte.vallet, Couleur.noir, Type.pique)
    case "dame pique" => PlayingCard.P(NumeroCarte.dame, Couleur.noir, Type.pique)
    case "roi pique" => PlayingCard.P(NumeroCarte.roi, Couleur.noir, Type.pique)


  abstract class comparator() extends scala.math.Ordering[PlayingCard]:
      def comparatorColor(): LazyList[PlayingCard] = ???
      def comparatorRang(): LazyList[PlayingCard] = ???

  def castEnum(): LazyList[PlayingCard] = PlayingCard.clone

  def takeCard(): LazyList[PlayingCard] = new LazyList(PlayingCard.P())

}
