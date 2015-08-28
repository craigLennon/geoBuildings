package interpret

import java.awt.{Point=>awtPoint}
import com.vividsolutions.jts.geom.util._
import com.vividsolutions.jts.geom._
/**
 * @author cLennon
 */
class Wall(ptList:LineString) {
    val g =new GeometryFactory 
    val lineStr=ptList
    var orientation=true
    var extent=1.0
    lazy val inside=innerSide(extent,orientation)
    
    
    def this(ptList:List[Coordinate]){this(  (new GeometryFactory).createLineString(ptList.toArray))} 
    
    def awtPointsLS:List[awtPoint]={
      def cd2pt(c:Coordinate):awtPoint=new awtPoint(c.x.toInt,c.y.toInt)
        val x=lineStr.getCoordinates()
        x.map(y=>cd2pt(y)).toList
    }
        def awtPointsLR:List[awtPoint]={
      def cd2pt(c:Coordinate):awtPoint=new awtPoint(c.x.toInt,c.y.toInt)
        val x=lineStr.getCoordinates()
        x.map(y=>cd2pt(y)).toList
    }
    
    
    def innerSide(d:Double,side:Boolean):Polygon={ //parameter is a proportion of the length 
        val ext=d*lineStr.getLength
        val tmp=lineStr
        val st=lineStr.getStartPoint
        val transTo=new AffineTransformation
        transTo.setToTranslation(-st.getX,-st.getY)
        val transBack=new AffineTransformation
        transBack.setToTranslation(st.getX,st.getY)
        tmp.apply(transTo)
        val end=tmp.getEndPoint
        val rot = new AffineTransformation
        val theta=math.atan2(end.getY, end.getX) 
        rot.setToRotation(-theta)  
        val rotBack = new AffineTransformation 
        rotBack.setToRotation(theta)
        tmp.apply(rot)     
        val nwStart=tmp.getStartPoint
        val nwEnd=tmp.getEndPoint 
        var cor1=new Coordinate(nwEnd.getX,nwEnd.getY-ext)
        var cor2=new Coordinate(nwStart.getX,nwStart.getY-ext)  
        if(side){
           cor1=new Coordinate(nwEnd.getX,nwEnd.getY+ext)
           cor2=new Coordinate(nwStart.getX,nwStart.getY+ext)
           }
        var newLs:Array[Coordinate]=tmp.getCoordinates
        val stcd=new Coordinate(nwStart.getCoordinate)
        newLs=newLs++:Array(cor1,cor2 ,stcd)
        val box=g.createLineString(newLs)
        box.apply(rotBack)
        box.apply(transBack)
        val ring= g.createLinearRing(box.getCoordinates)
        val poly=g.createPolygon(ring)
        return poly
    }

    def innerSide(side:Boolean):Polygon=innerSide(1.0,side:Boolean)
    
}