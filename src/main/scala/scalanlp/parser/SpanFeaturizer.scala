package scalanlp.parser

import projections.GrammarRefinements
import scalanlp.util.TypeTags._
import scalanlp.trees.{LexicalProduction, Production, Rule}
import scalanlp.util.{TypeTags, Index}

/**
 *
 * @author dlwh
 */

trait SpanFeaturizer[L, W, Feat] {
  def index: Index[Feat]
  
  def specialize(words: Seq[W]):Specialization
  
  trait Specialization {
    def words: Seq[W]

    def featuresForBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int):Array[Int]
    def featuresForUnaryRule(begin: Int, end: Int, rule: Int, ref: Int):Array[Int]
    def featuresForSpan(begin: Int, end: Int, tag: Int, ref: Int):Array[Int]
  }

}

object SpanFeaturizer {
  
  def forGrammar[L, W](grammar: Grammar[L], lexicon: Lexicon[L, W]):SpanFeaturizer[L, W, Production[L, W]] = {
    forRefinedGrammar(grammar, lexicon, GrammarRefinements.identity(grammar))
  }
  
  def forRefinedGrammar[L, L2, W](coarse: Grammar[L],
                                  lexicon: Lexicon[L, W],
                                  proj: GrammarRefinements[L, L2]):SpanFeaturizer[L, W, Production[L2, W]] = {

    val refinedTagWords = for {
      (l, w) <- lexicon.knownTagWords
      l2 <- proj.labels.refinementsOf(l)
    } yield LexicalProduction(l2, w)

    val ind = Index[Production[L2,W]](proj.rules.fineIndex ++ refinedTagWords)
    
    val refinedRuleIndices = Array.tabulate(coarse.index.size) { r =>
      proj.rules.refinementsOf(tag[Rule[L]](r)).map(fr => ind(proj.rules.fineIndex.get(fr)))
    }

    new SpanFeaturizer[L, W, Production[L2, W]] {
      def index = ind

      def specialize(w: Seq[W]) = new Specialization {
        val words = w

        def featuresForBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int) = {
          Array(refinedRuleIndices(rule)(ref))
        }

        def featuresForUnaryRule(begin: Int, end: Int, rule: Int, ref: Int) = {
          Array(refinedRuleIndices(rule)(ref))
        }

        def featuresForSpan(begin: Int, end: Int, tag: Int, ref: Int) = {
          if(begin+1 != end) Array.empty[Int]
          else {
            val refinedTag = proj.labels.globalize(tag, ref)
            val prod = LexicalProduction(proj.labels.fineIndex.get(refinedTag), words(begin))
            Array(ind(prod))
          }
        }
      }
    }
    
  }
}