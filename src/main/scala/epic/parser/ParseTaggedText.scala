package epic.parser

import epic.trees.{AnnotatedLabel, Tree}
import epic.util.ProcessTextMain
import epic.models.ParserSelector


object ParseTaggedText extends ProcessTextMain[Parser[AnnotatedLabel, String], Tree[AnnotatedLabel]] {


  override def render(model: Parser[AnnotatedLabel, String], ann: Tree[AnnotatedLabel], tokens: IndexedSeq[String]): String = {
    val words = tokens.map(TaggedToken(_)).map(_.word)
    ann.render(words, newline = false)
  }

  // assume each token is word/pos, i.e., the last / separates word and pos
  override def annotate(model: Parser[AnnotatedLabel, String], text: IndexedSeq[String]): Tree[AnnotatedLabel] = {
    val tokens = text.map(TaggedToken(_))
    val words = tokens.map(_.word)
    val poses = tokens.map(_.pos)

    def okTag(i: Int, tag: AnnotatedLabel) = AnnotatedLabel(poses(i)) == tag.baseAnnotatedLabel

    model.withGivenTags(words, okTag _)
  }

  override def classPathLoad(language: String): Parser[AnnotatedLabel, String] = {
    ParserSelector.loadParser(language).get
  }
}
