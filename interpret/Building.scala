package interpret
import com.vividsolutions.jts.geom._

/**
 * @author cLennon
 */
class Building(val name:String) {
  var insideArea:List[Polygon]=List()
  var walls:List[LineString]=List()
  var wallInsides :List[Polygon]=List()
  
  def joinWalls:Geometry={
  var base:Geometry=wallInsides(0)
  wallInsides.foreach{ w => base=base.union(w) }
  return base}
  
  def getHull:Polygon={
    val geo:Geometry=joinWalls.convexHull()
    geo.asInstanceOf[Polygon]
  }
 
}