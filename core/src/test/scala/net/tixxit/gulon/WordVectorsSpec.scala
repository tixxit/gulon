package net.tixxit.gulon

import java.io.{Reader, StringReader}
import java.nio.file.Files
import java.util.Arrays

import org.scalatest.FunSuite
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.PropertyChecks

class WordVectorsSpec extends FunSuite with PropertyChecks {
  import Generators._

  case class Word2Vec(
    wordVectors: WordVectors,
    contents: String
  ) {
    def newReader(): Reader = new StringReader(contents)
  }

  def genWord2Vec: Gen[Word2Vec] = for {
    maxSize <- Gen.size
    rows <- Gen.choose(1, math.max(1, maxSize))
    dim <- Gen.choose(1, 500)
    words0 <- genSetOfN(rows, Gen.identifier)
    vectors <- Gen.listOfN(rows, genPoint(dim))
  } yield {
    val words = words0.toList

    val data = Matrix(rows, dim, vectors.toArray)
    val wordVectors = WordVectors.Unindexed(words.toVector, data)

    val lines0 = words.zip(vectors).map {
      case (w, xs) => w + " " + xs.mkString(" ")
    }
    val lines = s"$rows $dim" :: lines0
    val content = lines.mkString("\n")

    Word2Vec(wordVectors, content)
  }

  implicit val arbWord2Vec: Arbitrary[Word2Vec] = Arbitrary(genWord2Vec)

  test("readWord2Vec reads word2vec") {
    forAll { (word2Vec: Word2Vec) =>
      val actual = WordVectors.readWord2Vec(word2Vec.newReader()).unsafeRunSync()
      assert(actual == word2Vec.wordVectors)
    }
  }

  test("readWord2VecPath reads UTF-8 word2vec") {
    forAll { (word2Vec: Word2Vec) =>
      val path = Files.createTempFile("test", "word2vec")
      try {
        Files.write(path, word2Vec.contents.getBytes("utf-8"))
        val actual = WordVectors.readWord2VecPath(path.toString).unsafeRunSync()
        assert(actual == word2Vec.wordVectors)
      } finally {
        Files.deleteIfExists(path)
      }
    }
  }

  test("readWord2Vec(normalized=true) normalizes vectors") {
    forAll { (word2Vec: Word2Vec) =>
      val actual = WordVectors.readWord2Vec(word2Vec.newReader(), normalize=true).unsafeRunSync()
      (0 until actual.size).foreach { i =>
        assert(TestUtils.nearlyEqual(MathUtils.distance(actual(i)), 1f))
      }
    }
  }

  test("sorted indexes the keys") {
    forAll { (word2Vec: Word2Vec) =>
      val orig = word2Vec.wordVectors
      val sorted = orig.sorted
      val keyIndex = sorted.keyIndex
      (0 until orig.size).foreach { origRow =>
        val word = orig.word(origRow)
        val expected = orig(origRow)
        val sortedRow = keyIndex.lookup(word).get
        val actual = sorted(sortedRow)
        assert(Arrays.equals(actual, expected))
      }
    }
  }

  val genWord2VecWithClustering = for {
    word2Vec <- genWord2Vec
    k <- Gen.sized(n => Gen.choose(2, math.max(2, n)))
    kmeans <- genKMeans(word2Vec.wordVectors.dimension, k)
  } yield (word2Vec, kmeans)

  test("grouped indexed the keys") {
    forAll(genWord2VecWithClustering) { case (word2Vec, kmeans) =>
      val orig = word2Vec.wordVectors
      val grouped = orig.grouped(kmeans).unsafeRunSync()
      val keyIndex = grouped.keyIndex
      (0 until orig.size).foreach { origRow =>
        val word = orig.word(origRow)
        val expected = orig(origRow)
        val groupedRow = keyIndex.lookup(word).get
        val actual = grouped(groupedRow)
        assert(Arrays.equals(actual, expected))
      }
    }
  }

  test("grouped residuals are residuals without centroids") {
    forAll(genWord2VecWithClustering) { case (word2Vec, kmeans) =>
      val grouped = word2Vec.wordVectors.grouped(kmeans).unsafeRunSync()
      val residuals = grouped.residuals
      (0 until grouped.size).foreach { row =>
        val expected = grouped.toMatrix.data(row)
        val residual = residuals.data(row)
        val centroid = grouped.centroids(grouped.clusterOf(row))
        val actual = MathUtils.add(centroid, residual)
        // This has a pretty high tolerance for equality. Going lower seems to
        // occasionally produce a float that is pretty awful, but it is quite
        // rare.
        assert(TestUtils.nearlyEqualArrays(actual, expected, 0.05f))
      }
    }
  }
}
