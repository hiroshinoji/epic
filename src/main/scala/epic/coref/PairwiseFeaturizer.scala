package epic.coref

import breeze.linalg.Counter
import epic.framework.Feature
import breeze.text.tokenize.PorterStemmer

/**
 *
 * @author dlwh
 */
trait PairwiseFeaturizer {
  def featuresFor(a: MentionCandidate, b: MentionCandidate, context: IndexedSeq[IndexedSeq[String]]):Counter[Feature, Double]
  def featuresForRoot(a: MentionCandidate, context: IndexedSeq[IndexedSeq[String]]):Counter[Feature, Double]
}

class SimplePairwiseFeaturizer extends PairwiseFeaturizer {
  def featuresFor(a: MentionCandidate,
                  b: MentionCandidate,
                  context: IndexedSeq[IndexedSeq[String]]): Counter[Feature, Double] = {
    val res = Counter[Feature, Double]()

    val tpeA = mentionType(a)
    val tpeB = mentionType(b)
    val typeFeature = PairFeature(tpeA, tpeB)
    res(typeFeature) = 1.0

    // exact match
    if(a.words == b.words) {
      res(PairFeature(typeFeature, ExactStringMatch)) = 1.0
    }

    val aLower = a.words.map(_.toLowerCase)
    val bLower = b.words.map(_.toLowerCase)
    if(aLower == bLower) {
      res(PairFeature(typeFeature, LowerStringMatch)) = 1.0
    }

    val aLSet = a.words.toSet
    val bLSet = b.words.toSet

    // head match
    val headA = semHead(a.words)
    val headB = semHead(b.words)
    if(headA == headB) {
      res(PairFeature(HeadMatch, typeFeature)) = 1.0
    } else if (PorterStemmer(headA) == PorterStemmer(headB)) {
      res(PairFeature(HeadStemMatch, typeFeature)) = 1.0
    } else if (bLSet.contains(headA)) {
      res(PairFeature(ContainsHeadR,typeFeature)) = 1.0
    } else if (aLSet.contains(headB)) {
      res(PairFeature(ContainsHeadL,typeFeature)) = 1.0
    } else {
      res(Not(PairFeature(HeadMatch, typeFeature))) = 1.0
    }

    // hack to get coordinate:
    if(a.words.contains("and") || a.words.contains("both")) {
      if(tpeB == 'Pronoun)
        res(PairFeature(CoordinationL,headB)) = 1.0
      else tpeB
        res(PairFeature(CoordinationL,typeFeature)) = 1.0
    }

    if(b.words.contains("and") || b.words.contains("both")) {
      if(tpeA == 'Pronoun)
        res(PairFeature(CoordinationR,headA)) = 1.0
      else tpeA
        res(PairFeature(CoordinationR,typeFeature)) = 1.0
    }


    // string match
    if(tpeA != 'Pronoun && tpeB != 'Pronoun) {
      val aSet = a.words.toSet
      val bSet = b.words.toSet
      val inter = (aSet & bSet)
      res(PartialStringMatch('Jacard)) = inter.size.toDouble / (aSet | bSet).size

      val interL = (aSet & bSet)
      res(PartialStringMatch('JacardLower)) = interL.size.toDouble / (aLSet | bLSet).size

      if(inter.isEmpty)
        res(NoOverlap) = 1.0
      else if(inter == aSet || inter == bSet) {
        res(ContainsString) = 1.0
      }
    }

    // discourse-y stuff
    val distance = binnedSentenceDistance(a.sentence, b.sentence)
    res(SentenceDistance(distance)) = 1.0
    res(PairFeature(SentenceDistance(distance), typeFeature)) = 1.0

    // pronouns agreement and such.
    if(tpeB == 'Pronoun) {
      val pronHeadF = PairFeature(tpeA, headB)
      res(pronHeadF) = 1.0
      res(PairFeature(pronHeadF, SentenceDistance(distance))) = 1.0
    }

    if(tpeA == 'Pronoun) {
      val pronHeadF = PairFeature(headA, tpeB)
      res(pronHeadF) = 1.0
      res(PairFeature(pronHeadF, SentenceDistance(distance))) = 1.0
      if(tpeB == 'Pronoun) {
        val pronounHeads = PairFeature(headA, headB)
        res(pronounHeads) = 1.0
        res(PairFeature(pronounHeads, SentenceDistance(distance))) = 1.0
      }
    }

    // random crap
    if(a.sentence == b.sentence && (b.span.contains(a.span) || a.span.contains(b.span))) {
      res(SpanContained) = 1.0
    }


    res
  }

  def featuresForRoot(a: MentionCandidate, context: IndexedSeq[IndexedSeq[String]]): Counter[Feature, Double] = {
    val res = Counter[Feature, Double]()
    res(PairFeature('Root, mentionType(a))) = 1.0
    res(PairFeature(PairFeature('Root, mentionType(a)),SentenceDistance(binnedSentenceDistance(0, a.sentence)))) = 1.0


    res
  }

  def mentionType(a: MentionCandidate) = {
    if (a.words.length == 1 && isPronoun(a.words(0))) 'Pronoun
    else if (a.words(0)(0).isUpper) 'Proper
    else 'Nominal
  }


  def isPronoun(word: String) = pronouns.contains(word)
  def isQuote(s: String) = s != "``" && s != "''"

  private def binnedSentenceDistance(a: Int, b: Int) = {
    val dist = b - a
    if(dist == 0) 0
    else if(dist == 4) 1
    else if(dist < 5) 2
    else if(dist < 10) 3
    else 4
  }

  private def semHead(mention: IndexedSeq[String]) = {
    if(mention.length == 1) mention.head.toLowerCase
    else {
      var i = mention.indexWhere(s => prepositions.contains(s) || !(s(0).isLetterOrDigit && !isQuote(s)))
      if(i <= 0)
        i = mention.length
      val withoutPrep = mention.view(0, i)
      withoutPrep.reverseIterator.dropWhile(s => !s(0).isLetterOrDigit).next().toLowerCase
    }

  }

  val prepositions = Set("aboard",
  "about",
  "above",
  "across",
  "after",
  "against",
  "along",
  "amid",
  "among",
  "anti",
  "around",
  "as",
  "at",
  "before",
  "behind",
  "below",
  "beneath",
  "beside",
  "besides",
  "between",
  "beyond",
  "but",
  "by",
  "concerning",
  "considering",
  "despite",
  "down",
  "during",
  "except",
  "excepting",
  "excluding",
  "following",
  "for",
  "from",
  "in",
  "inside",
  "into",
  "like",
  "minus",
  "near",
  "of",
  "off",
  "on",
  "onto",
  "opposite",
  "outside",
  "over",
  "past",
  "per",
  "plus",
  "regarding",
  "round",
  "save",
  "since",
  "than",
  "through",
  "to",
  "toward",
  "towards",
  "under",
  "underneath",
  "unlike",
  "until",
  "up",
  "upon",
  "versus",
  "via",
  "with",
  "within",
  "without")

  val pronouns = Set(
    "He", "She", "They", "It", "You", "I", "We", "Me", "Us", "Him", "Her", "Them",
    "he", "she", "they", "it", "you", "i", "we", "me", "us", "him", "her", "them",
    "His", "Her", "Their", "Its", "Your", "My", "Our",
    "his", "her", "their", "its", "your", "my", "our",
  "Himself", "Herself", "Themselves", "Itself", "Yourself", "yourselves", "Myself", "Ourselves",
  "himself", "herself", "themselves", "itsself", "yourselves", "myself", "ourselves",
  "This","this","These","Those","that","these","those","that","both"
  )



}
