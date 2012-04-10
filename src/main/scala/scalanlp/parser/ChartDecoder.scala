package scalanlp.parser


import java.util.Arrays
import projections.{AnchoredPCFGProjector, AnchoredRuleMarginalProjector}
import scalanlp.trees._
import scalanlp.collection.mutable.TriangularArray
import scalanlp.util._
import scalala.library.Numerics._
import TypeTags._


/**
 * A ChartDecoder can turn an inside chart (and optionally an outside chart) from some
 * parse chart over symbols F into a tree over symbols C
 *
 * @author dlwh
 */
trait ChartDecoder[L, W] extends Serializable{
  def extractBestParse(marginal: ChartMarginal[ParseChart, L, W]):BinarizedTree[L]
}

/**
 * Tries to extract a tree that maximizes log score.
 */
@SerialVersionUID(2)
class ViterbiDecoder[L, W] extends ChartDecoder[L, W] with Serializable {

  override def extractBestParse(marginal: ChartMarginal[ParseChart, L, W]):BinarizedTree[L] = {
    import marginal._
    val labelIndex = grammar.labelIndex
    val rootIndex = (grammar.labelIndex(grammar.root))

    def buildTreeUnary(begin: Int, end:Int, root: Int, rootRef: Int):BinarizedTree[L] = {
      var maxScore = Double.NegativeInfinity
      var maxChild = -1
      var maxChildRef = -1
      for {
        r <- grammar.grammar.indexedUnaryRulesWithParent(root)
        refR <- spec.validRuleRefinementsGivenParent(begin, end, r, rootRef)
      } {
        val ruleScore = spec.scoreUnaryRule(begin, end, r, refR)
        val b = grammar.grammar.child(r)
        val refB = spec.childRefinement(r, refR)
        val score = ruleScore + inside.bot(begin, end, b, refB)
        if(score > maxScore) {
          maxScore = score
          maxChild = b
          maxChildRef = refB
        }
      }

      if(maxScore == Double.NegativeInfinity) {
        println("entered things: " + inside.bot.enteredLabelScores(begin, end).map { case (i, v) => (grammar.labelIndex.get(i), v)}.toList)
        sys.error("Couldn't find a tree!" + begin + " " + end + " " + grammar.labelIndex.get(root))
      }
      val child = buildTree(begin, end, maxChild, maxChildRef)
      UnaryTree(labelIndex.get(root), child)(Span(begin, end))
    }

    def buildTree(begin: Int, end: Int, root: Int, rootRef: Int):BinarizedTree[L] = {
      var maxScore = Double.NegativeInfinity
      var maxLeft = -1
      var maxRight = -1
      var maxLeftRef = -1
      var maxRightRef = -1
      var maxSplit = -1
      if(begin +1 == end) {
        return NullaryTree(labelIndex.get(root))(Span(begin, end))
      }

      val spanScore = spec.scoreSpan(begin, end, root, rootRef)
      for {
        r <- grammar.grammar.indexedBinaryRulesWithParent(root)
        b = grammar.grammar.leftChild(r)
        c = grammar.grammar.rightChild(r)
        refR <- spec.validRuleRefinementsGivenParent(begin, end, r, rootRef)
        refB = spec.leftChildRefinement(r, refR)
        refC = spec.rightChildRefinement(r, refR)
        split <- inside.top.feasibleSpan(begin, end, b, refB, c, refC)
      } {
        val ruleScore = spec.scoreBinaryRule(begin, split, end, r, refR)
        val score = (
          ruleScore
            + inside.top.labelScore(begin, split, b, refB)
            + inside.top.labelScore(split, end, c, refC)
            + spanScore
          )
        if(score > maxScore) {
          maxScore = score
          maxLeft = b
          maxLeftRef = refB
          maxRight = c
          maxRightRef = refC
          maxSplit = split
        }
      }

      if(maxScore == Double.NegativeInfinity) {
        println("entered things: " + inside.bot.enteredLabelScores(begin, end).map { case (i, v) => (grammar.labelIndex.get(i), v)}.toList)
        sys.error("Couldn't find a tree!" + begin + " " + end + " " + grammar.labelIndex.get(root))
      } else {
        val lchild = buildTreeUnary(begin, maxSplit, maxLeft, maxLeftRef)
        val rchild = buildTreeUnary(maxSplit, end, maxRight, maxRightRef)
        BinaryTree(labelIndex.get(root), lchild, rchild)(Span(begin, end))
      }


    }

    val maxRootRef = spec.validLabelRefinements(0, inside.length, rootIndex).maxBy(ref => inside.top(0, inside.length, rootIndex, ref))
    val t = buildTreeUnary(0, inside.length, rootIndex, maxRootRef)
    t
  }
}

/**
 * Tries to extract a tree that maximizes rule product in the coarse grammar
 **/
case class MaxRuleProductDecoder[L, W](grammar: Grammar[L]) extends ChartDecoder[L, W] {
  val p = new AnchoredRuleMarginalProjector[L,W]()

  def extractBestParse(marginal: ChartMarginal[ParseChart, L, W]) = {
    val scorer = p.buildSpanScorer(marginal)
    val oneoff: WeightedGrammar[L, W] = WeightedGrammar.oneOff(grammar, scorer)
    val newMarg = new CKYChartBuilder(oneoff, ParseChart.logProb).charts(marginal.spec.words)
    val tree = new ViterbiDecoder[L,W].extractBestParse(newMarg)
    tree
  }
}

/**
 * Tries to extract a tree that maximizes... XXX
 **/
class MaxVariationalDecoder[L, W](grammar: Grammar[L]) extends ChartDecoder[L, W] {
  val p = new AnchoredPCFGProjector[L,W](grammar)

  def extractBestParse(marginal: ChartMarginal[ParseChart, L, W]) = {
    val scorer = p.buildSpanScorer(marginal)
    val oneoff: WeightedGrammar[L, W] = WeightedGrammar.oneOff(grammar, scorer)
    val newMarg = new CKYChartBuilder(oneoff, ParseChart.logProb).charts(marginal.spec.words)
    val tree = new ViterbiDecoder[L,W].extractBestParse(newMarg)
    tree
  }
}

@SerialVersionUID(2L)
class MaxConstituentDecoder[L, W] extends ChartDecoder[L, W] {

  def extractBestParse(marginal: ChartMarginal[ParseChart, L, W]) = {
    import marginal._

    val labelIndex = marginal.grammar.labelIndex

    val maxSplit = new TriangularArray[Int](inside.length+1, 0)
    val maxBotLabel = new TriangularArray[Int](inside.length+1, -1)
    val maxBotScore = new TriangularArray[Double](inside.length+1, Double.NegativeInfinity)
    val maxTopLabel = new TriangularArray[Int](inside.length+1, -1)
    val maxTopScore = new TriangularArray[Double](inside.length+1, Double.NegativeInfinity)


    val scores = spec.grammar.labelEncoder.fillArray(Double.NegativeInfinity)
    val buffer = Array.fill(1000)(Double.NegativeInfinity)

    def marginalizeRefinements(begin: Int, end: Int, l: Int, ichart: inside.ChartScores, ochart: outside.ChartScores): Double = {
      var bufOff = 0
      for (lRef <- ichart.enteredLabelRefinements(begin, end, l)) {
        val myScore = ichart.labelScore(begin, end, l, lRef) + ochart.labelScore(begin, end, l, lRef) - partition
        buffer(bufOff) = myScore
        bufOff += 1
      }
      inside.sum(buffer, bufOff)
    }

    for(i <- 0 until inside.length) {
      Arrays.fill(scores, Double.NegativeInfinity)
      for(l <- inside.bot.enteredLabelIndexes(i, i + 1)) {
        scores(l) = marginalizeRefinements(i, i + 1, l, inside.bot, outside.bot)
      }
      maxBotScore(i, i + 1) = scores.max
      maxBotLabel(i, i + 1) = scores.argmax

      Arrays.fill(scores, Double.NegativeInfinity)
      for(l <- inside.top.enteredLabelIndexes(i, i + 1)) {
        scores(l) = marginalizeRefinements(i, i + 1, l, inside.top, outside.top)
      }
      maxTopScore(i, i + 1) = logSum(scores.max, maxBotScore(i, i + 1))
      maxTopLabel(i, i + 1) = scores.argmax
    }

    for {
      span <- 2 to inside.length
      begin <- 0 to (inside.length - span)
      end = begin + span
    } {
      Arrays.fill(scores, Double.NegativeInfinity)
      for(l <- inside.bot.enteredLabelIndexes(begin, end)) {
        scores(l) = marginalizeRefinements(begin, end, l, inside.bot, outside.bot)
      }
      maxBotScore(begin, end) = scores.max
      maxBotLabel(begin, end) = scores.argmax

      Arrays.fill(scores, Double.NegativeInfinity)
      for(l <- inside.top.enteredLabelIndexes(begin, end)) {
        scores(l) = marginalizeRefinements(begin, end, l, inside.top, outside.top)
      }
      maxTopScore(begin, end) = logSum(scores.max, maxBotScore(begin, end))
      maxTopLabel(begin, end) = scores.argmax

      val (split, splitScore) = (for(split <- begin +1 until end) yield {
        val score = logSum(maxTopScore(begin, split), maxTopScore(split, end))
        (split, score)
      }).maxBy(_._2)

      maxSplit(begin, end) = split
      maxTopScore(begin, end) = logSum(maxTopScore(begin, end), splitScore)
      //maxBotScore(begin, end) = logSum(maxBotScore(begin, end), splitScore)
    }

    def extract(begin: Int, end: Int):BinarizedTree[L] = {
      val lower = if(begin + 1== end) {
        if(maxBotScore(begin, end) == Double.NegativeInfinity)
          throw new RuntimeException("Couldn't make a good score for " + (begin, end) + ". InsideIndices: " + inside.bot.enteredLabelIndexes(begin, end).toIndexedSeq + " outside: " + outside.bot.enteredLabelIndexes(begin, end).toIndexedSeq)
        NullaryTree(labelIndex.get(maxBotLabel(begin, end)))(Span(begin, end))
      } else {
        val split = maxSplit(begin, end)
        val left = extract(begin, split)
        val right = extract(split, end)
        BinaryTree(labelIndex.get(maxBotLabel(begin, end)), left, right)(Span(begin, end))
      }

      UnaryTree(labelIndex.get(maxTopLabel(begin, end)), lower)(Span(begin, end))
    }

    extract(0, inside.length)
  }
}
