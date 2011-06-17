package scalanlp.parser

import projections._;
import scalanlp.trees._;
import java.io.File
import scalanlp.data.Example

/**
 * Represents a treebank with attendant spans, binarization, etc. Used in all the parser trainers.
 *
 * @author dlwh
 */
case class ProcessedTreebank(treebank: TreebankParams, spans: SpanParams) {
  import spans._;

  lazy val trainTreesWithUnaries = transformTrees(treebank.treebank.train, trainSpans);
  lazy val (trainTrees,replacer) = removeUnaryChains(trainTreesWithUnaries);

  lazy val devTrees = transformTrees(treebank.treebank.dev, devSpans);

  lazy val testTrees = transformTrees(treebank.treebank.test, testSpans);


  def transformTrees(portion: treebank.treebank.Portion,
                     spans: Iterable[SpanScorer[String]]): IndexedSeq[TreeInstance[String,String]] = {
    import treebank._;

    val binarizedAndTransformed = for (
      (((tree, words),span),index) <- (portion.trees zip spans.iterator).zipWithIndex if words.length <= maxLength
    ) yield {
      val transformed = xform(tree);
      val vertAnnotated = Trees.annotateParents(transformed,depth = verticalMarkovization);
      val bin = binarize(vertAnnotated);
      val horiz = Trees.markovizeBinarization(bin,horizontalMarkovization);
      TreeInstance(portion.name +"-" + index,horiz,words,span)
    }

    binarizedAndTransformed.toIndexedSeq
  }

  def removeUnaryChains(trees: IndexedSeq[TreeInstance[String,String]]) = {
    val chainRemover = new UnaryChainRemover[String];

    val (dechained,chainReplacer) = chainRemover.removeUnaryChains(trees.iterator.map { ti => (ti.tree,ti.words)})

    val dechainedWithSpans = for {
      ((t,w),TreeInstance(id,_,_,span)) <- (dechained zip trees)
    } yield TreeInstance(id,t,w,span);

    (dechainedWithSpans, chainReplacer)
  }

}
case class TreebankParams(path: File,
                          maxLength:Int = 40,
                          binarization:String = "xbar",
                          processing: String = "standard",
                          verticalMarkovization:Int=0,
                          horizontalMarkovization:Int=1000) {
  def binarize = {
    if(binarization == "xbar") Trees.xBarBinarize(_:Tree[String],left=false);
    else if(binarization == "leftXbar") Trees.xBarBinarize(_:Tree[String],left=true);
    else Trees.binarize(_:Tree[String]);
  }

  val treebank = {
    if(path.isDirectory) Treebank.fromPennTreebankDir(path)
    else DenseTreebank.fromZipFile(path);
  }

  val xform = processing match {
    case "german" => Trees.Transforms.GermanTreebankTransform;
    case _ => Trees.Transforms.StandardStringTransform;
  }



}

case class SpanParams(directory: File = null) {
  def loadSpans(path: Option[File]):Iterable[SpanScorer[String]] = path match {
    case Some(path) => ProjectTreebankToLabeledSpans.loadSpansFile(path);
    case None =>       Stream.continually(SpanScorer.identity)
  }

  val trainSpansFile = Option(directory).map{dir => new File(dir, ProjectTreebankToLabeledSpans.TRAIN_SPANS_NAME)}
  lazy val trainSpans = loadSpans(trainSpansFile);

  val devSpansFile = Option(directory).map{dir => new File(dir, ProjectTreebankToLabeledSpans.DEV_SPANS_NAME)}
  lazy val devSpans = loadSpans(devSpansFile);

  val testSpansFile = Option(directory).map{dir => new File(dir, ProjectTreebankToLabeledSpans.TEST_SPANS_NAME)}
  lazy val testSpans = loadSpans(testSpansFile);
}

case class TreeInstance[L,+W](id: String,
                             tree: BinarizedTree[L],
                             words: Seq[W],
                             spanScorer: SpanScorer[L] = SpanScorer.identity[L]) extends Example[Tree[L],(Seq[W],SpanScorer[L])] {
  def label = tree;
  def features = words -> spanScorer;
}

