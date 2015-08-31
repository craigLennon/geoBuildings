package interpret

import java.awt.{Point=>awtPoint}
import com.vividsolutions.jts.geom.util._
import com.vividsolutions.jts.geom._
import com.cra.figaro.language._
import com.cra.figaro.library.collection._
import com.cra.figaro.library.compound._
import com.cra.figaro.library.atomic.discrete.{Uniform=>dUniform}
/**
 * @author cLennon
 */
class Wall(ptList:LineString) {
    val g =new GeometryFactory 
    val lineStr=ptList
    var orientation:Element[Boolean]=Flip(.5)
    var extent:Element[Double]= dUniform(0.01,0.1,0.3,0.5,0.75,1.0,1.5,2.0)
    var inner:Element[Polygon]=rInnerSide
    
    
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
    
   
   def rInnerSide:Element[Polygon]={   
      println("made it to rInnerSide")
      val in:Element[Polygon]=Apply(this.extent,this.orientation,(d:Double,b:Boolean)=> innerSide(d, b))
      return in
       }     
    
    def innerSide(d:Double,side:Boolean):Polygon={ //parameter is a proportion of the length 
      println("made it to innerSide")
      println(d)
      println(side)
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
        println(poly)
        return poly
    }

    //def rInnerSide(side:Element[Boolean):Polygon=innerSide(this.extent,side:Boolean)
    //def rInnerSide:Polygon=innerSide(this.orientation)
}
