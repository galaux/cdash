import org.scalatest._

class RowSelectionSpec extends FlatSpec with Matchers {

  val metric1 = Metric("m1", AggregationOperation.Sum)
  val metric2 = Metric("m2", AggregationOperation.Average)
  val dimension1 = Dimension("d1")
  val dimension2 = Dimension("d2")

  val selection = RowSelection(
    Seq(metric1, metric2),
    Seq(dimension1, dimension2),
    Seq(IndexedSeq(1.0, 3.0), IndexedSeq(2.0, 9.0)),
    Seq(IndexedSeq("dimVal11", "dimVal12"), IndexedSeq("dimVal21", "dimVal22"))
  )

//  describe("RowSelection") {

    it should "take the n first rows" in {
      val taken = selection.take(1)
      taken.metricRows should have size(1)
      taken.dimensionRows should have size(1)
    }

    it should "add a selection to another selection" in {
      val other = selection
      val added = selection ++ other
      added.metricHeaders should equal(selection.metricRows)
      added.dimensionHeaders should equal(selection.dimensionRows)
      added.metricRows should equal(Seq(
        IndexedSeq(1.0, 3.0),
        IndexedSeq(2.0, 7.0),
        IndexedSeq(1.0, 3.0),
        IndexedSeq(2.0, 7.0)))
      added.dimensionRows should equal(Seq(
        IndexedSeq("dimVal11", "dimVal12"),
        IndexedSeq("dimVal21", "dimVal22"),
        IndexedSeq("dimVal11", "dimVal12"),
        IndexedSeq("dimVal21", "dimVal22")))
    }

    it should "filter the selection" in {
      val f: (String => Boolean) = dimVal => (dimVal == "dimVal11")
      val filtered = selection.filter(dimension1, f)
      filtered.metricRows should equal(Seq(IndexedSeq(1.0, 3.0)))
      filtered.dimensionRows should equal(Seq(IndexedSeq("dimVal11", "dimVal12")))
    }

    it should "group by columns" in {
      val s = selection.copy(dimensionRows = Seq(
        IndexedSeq("dimVal1", "dimVal2"),
        IndexedSeq("dimVal1", "dimVal2")))
      val grouped = s.groupByColumns
      grouped.dimensionRows should equal(Seq(IndexedSeq("dimVal1", "dimVal2")))
      grouped.metricRows should equal(Seq(IndexedSeq(4.0, 6.0)))
    }

//  }

}
