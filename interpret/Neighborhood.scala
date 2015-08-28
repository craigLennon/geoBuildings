package interpret
 
import org.geotools.data.shapefile._
import java.io.File
import java.io.{File,Serializable=>JSerial}
import org.geotools.data.simple._
import org.opengis.feature.simple._
import org.geotools.data._
import org.geotools.data.store._
import java.util.{HashMap=>JHashMap,Map=>JMap}
import com.vividsolutions.jts.geom._
/**
 * @author cLennon
 */
class Neighborhood {
         val ptBuilder:SimpleFeatureType=DataUtilities.createType("Observation",
            "the_geom:Point:srid=4326,"+"dataType:String")//+"id:Int")
        val lnBuilder:SimpleFeatureType=DataUtilities.createType("Location",
            "the_geom:LineString:srid=4326," +"dataType:String")
  var walls:List[LineString]=List()
  var obs:List[(Point,String)]=List()
            
 def loadObsPts(shapeName:String){     
    val file:File= new File(shapeName)
    val map:JMap[String,JSerial] =new JHashMap()
    map.put("url",file.toURI().toURL().asInstanceOf[JSerial])
    val dataStore:DataStore=DataStoreFinder.getDataStore(map )
     //:SimpleFeatureSource 
    val typeNameSS=dataStore.getTypeNames()(0)
    println(typeNameSS)
    val typeName:String=dataStore.getTypeNames()(0)
    val featureSource= dataStore.getFeatureSource("InOutPts" );
    val featureCollection:SimpleFeatureCollection = featureSource.getFeatures();
    //val dat=factory.createDataStore(file.toURI().toURL())
     val  iterator:SimpleFeatureIterator = featureCollection.features();
     var ptList:List[Point]=List()
     var typeList:List[String]=List()
 try {
     while( iterator.hasNext() ){
           val feature: SimpleFeature = iterator.next();
          println( feature.getAttribute("the_geom") )
          println( feature.getAttribute("dataType"))
          val d=feature.getAttribute("dataType").asInstanceOf[String]
          val x=feature.getAttribute("the_geom").asInstanceOf[Point]
          ptList=ptList:+x
          typeList=typeList:+d
     }
 }
 finally {
   println(ptList)
   println(typeList)
     iterator.close();
 }
         obs=ptList zip typeList
  } 
  def loadWalls(shapeName:String ){
    val file:File= new File(shapeName)
    val map:JMap[String,JSerial] =new JHashMap()
    map.put("url",file.toURI().toURL().asInstanceOf[JSerial])
    val dataStore:DataStore=DataStoreFinder.getDataStore(map )
     //:SimpleFeatureSource 
    val typeNameSS=dataStore.getTypeNames()(0)
    println(typeNameSS)
    val typeName:String=dataStore.getTypeNames()(0)
    val featureSource= dataStore.getFeatureSource("Walls" );
    val featureCollection:SimpleFeatureCollection = featureSource.getFeatures();
    //val dat=factory.createDataStore(file.toURI().toURL())
     val  iterator:SimpleFeatureIterator = featureCollection.features();
     var lnList:List[LineString]=List()
      
 try {
     while( iterator.hasNext() ){
           val feature: SimpleFeature = iterator.next();
          println( feature.getAttribute("the_geom") )
         
           
          val x=feature.getAttribute("the_geom").asInstanceOf[MultiLineString]
          val L=x.getLength
          println(L)
          val z=for(n <- 0 to L.toInt) yield x.getGeometryN(n).asInstanceOf[LineString]
          println(z)
          lnList=lnList++z.toList
          
           
     }
 }
 finally {
   println(lnList)
 
     iterator.close();
 }
    walls=lnList
    println(walls)
  }
 
  
  var unassignedWalls:List[Wall]=List()
  var insidePt:List[Point] =List()
  var outsidePt:List[Point] =List()
  var build:List[Building]=List()
  def assignOrient(bs:List[Boolean]){val pairs=unassignedWalls.zip(bs)
        pairs.foreach(x=>x._1.orientation=x._2)  
  }
  def assignExtent(ds:List[Double]){val pairs=unassignedWalls.zip(ds)
        pairs.foreach(x=>x._1.extent=x._2)  
  }
  def assignOrientExtent(ds:List[Double],bs:List[Boolean]):Neighborhood={
      val nwN=this
      nwN.assignOrient(bs)
      nwN.assignExtent(ds)
      return nwN
  }
}
