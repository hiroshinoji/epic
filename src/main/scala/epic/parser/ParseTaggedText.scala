package epic.parser

import epic.trees.{AnnotatedLabel, Tree}
import epic.util.ProcessTextMain
import epic.models.ParserSelector


case class TaggedToken(word: String, pos: String)

object TaggedToken {
  def apply(taggedWord: String): TaggedToken = {
    val lastSlash = taggedWord.lastIndexOf('/')
    val word = taggedWord.take(lastSlash)
    val pos = taggedWord.drop(lastSlash + 1)
    TaggedToken(word, pos)
  }
}

object ParseTaggedText extends ProcessTextMain[Parser[AnnotatedLabel, String], Tree[AnnotatedLabel]] {


  override def render(model: Parser[AnnotatedLabel, String], ann: Tree[AnnotatedLabel], tokens: IndexedSeq[String]): String = {
    val words = tokens.map(TaggedToken(_)).map(_.word)
    ann.render(words, newline = false)
  }

  var strToLabel: Map[String, AnnotatedLabel] = null

  // assume each token is word/pos, i.e., the last / separates word and pos
  override def annotate(model: Parser[AnnotatedLabel, String], text: IndexedSeq[String]): Tree[AnnotatedLabel] = {
    val tokens = text.map(TaggedToken(_))
    val words = tokens.map(_.word)
    val poses = tokens.map(_.pos)

    if (!words.forall(!_.isEmpty)) sys.error(s"Input $text is not tagged correctly!")

    if (strToLabel == null) strToLabel = {
      val index = model.lexicon.labelIndex
      index.pairs.map { case (l, i) => l.baseAnnotatedLabel + "" -> index.get(i) }.toMap
    }
    def goldTags = poses map strToLabel

    model.withGivenTags(words, goldTags)
  }

  override def classPathLoad(language: String): Parser[AnnotatedLabel, String] = {
    ParserSelector.loadParser(language).get
  }
}
