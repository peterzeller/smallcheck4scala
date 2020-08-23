package smallcheck
import scala.collection.mutable.ListBuffer

/**
 * A collection of properties, which itself is a property.
 */
class Properties(val name: String) extends Property {
  import Property._
  
  private val props: ListBuffer[(String, Property)] = new ListBuffer
  
  private def oneProperty: Property = all(props.toSeq.map(_._2):_*)
  
  def apply(d: Int): Seq[TestCase] = oneProperty(d)
  
  def properties: Seq[(String, Property)] = props.toSeq
  
  def include(ps: Properties): Unit = for ((n, p) <- ps.properties) property(n) = p
  
  class PropertySpecifier() {
    def update(propName: String, p: Property): Unit = props += ((name+"."+propName, p))
  }
  
  lazy val property = new PropertySpecifier()
}
