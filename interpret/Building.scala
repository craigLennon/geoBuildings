package interpret
import com.vividsolutions.jts.geom.{Point=>JPoint,_}

/**
 * @author cLennon
 */
class Building {
  var insidePts:List[JPoint]=List()
  var walls:List[Wall]=List()
  var interior:List[Polygon]=List()
  
  def consistentInside:Boolean={
    if(interior.length==0|insidePts.length==0){return true}  
    var int:Polygon=interior(0)
    interior.foreach((x:Polygon)=>int.union(x))
    val contained=insidePts.map(x=>int.contains(x))
    val b=contained.contains(false)
    return !b
  }
    def consistentOutside(out:List[JPoint]):Boolean={
    if(interior.length==0|out.length==0){return true}  
    var int:Polygon=interior(0)
    interior.foreach((x:Polygon)=>int.union(x))
    val contained=out.map(x=>int.contains(x))
    val b=contained.contains(true)
    return !b
  }
}