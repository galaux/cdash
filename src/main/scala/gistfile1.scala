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

  def take(n: Int): RowSelection = {
    // To be implemented
  }

  def ++(other: RowSelection): RowSelection = {
    // To be implemented
  }

  def filter(col: Dimension, f: (String) => Boolean): RowSelection = {
    // To be implemented
  }

  def groupByColumns(cols: Seq[Dimension]): RowSelection = {
    // To be implemented
  }
}
