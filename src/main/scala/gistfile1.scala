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

  def groupByColumns: RowSelection = {
    // To be implemented
    RowSelection(Seq(), Seq(), Seq(), Seq())
  }
}
