object AggregationOperation extends Enumeration {
  type AggregationOperation = Value
  val Sum, Average, Min, Max, Count, Calculated = Value
}

case class Metric(id: String, aggregationOperation: AggregationOperation.Value)

case class Dimension(id: String)

case class RowSelection(
  metricHeaders   : Seq[Metric],
  dimensionHeaders: Seq[Dimension],
  metricRows      : Seq[IndexedSeq[Double]],
  dimensionRows   : Seq[IndexedSeq[String]]) {

  def take(n: Int): RowSelection = this.copy(
    metricRows    = metricRows   .take(n),
    dimensionRows = dimensionRows.take(n)
  )

  def ++(other: RowSelection): RowSelection = this.copy(
    metricRows    = metricRows    ++ other.metricRows,
    dimensionRows = dimensionRows ++ other.dimensionRows
  )

  def filter(col: Dimension, f: (String) => Boolean): RowSelection = {
    val colIndex = dimensionHeaders.indexOf(col)
    val (newDR, newMR) = (dimensionRows zip metricRows).filter {
      dimRow => f(dimRow._1(colIndex))
    }.unzip

    this.copy(metricRows = newMR, dimensionRows = newDR)
  }

  def groupByColumns(cols: Seq[Dimension]): RowSelection = {

    def getValueForIndexes(indexes: Seq[Int], dimRow: IndexedSeq[String]): IndexedSeq[String] =
      indexes.toIndexedSeq.map { idx => dimRow(idx) }

    def applyOp(op: AggregationOperation.Value, vals: Seq[Double]) = {
      op match {
        case AggregationOperation.Sum         => vals.sum
        case AggregationOperation.Average     => vals.sum / vals.length // TODO handle /0
        case AggregationOperation.Min         => 0 // TODO
        case AggregationOperation.Max         => 0 // TODO
        case AggregationOperation.Count       => 0 // TODO
        case AggregationOperation.Calculated  => 0 // TODO
      }
    }

    val indexesForCols = cols.map { dimensionHeaders.indexOf(_) }
    val gpRes = (dimensionRows zip metricRows).groupBy( _ match {
        case (dimensionRow, metricRow) => getValueForIndexes(indexesForCols, dimensionRow)
      }
    )

    val newMR = gpRes.mapValues { res =>
      val values = (res.map { _._2 }).transpose
      (metricHeaders zip values).map { _ match {
        case (metricHeader, value) => applyOp(metricHeader.aggregationOperation, value)
      }}.toIndexedSeq
    }

    this.copy(dimensionRows = newMR.keys.toSeq, metricRows = newMR.values.toSeq)
  }
}
