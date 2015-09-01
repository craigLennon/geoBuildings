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
import com.cra.figaro.language._
import com.cra.figaro.library.collection._
import com.cra.figaro.library.compound._
import com.cra.figaro.library.atomic.discrete.{Uniform=>dUniform}
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.algorithm._
/**
 * @author cLennon
 */
class Neighborhood {
  var walls:List[LineString]=List()
  var obs:List[(Polygon,String)]=List()  
  var unassignedWalls:List[Wall]=List()
  var insidePt:List[Polygon] =List()
  var outsidePt:List[Polygon] =List()
  var build:List[Building]=List()
  
  
//reasoning
def constrain{
    insidePt=assignInside
    outsidePt=assignOutside
   // println(outsidePt)
    setConstraints(unassignedWalls)
   
    def intersectList(py:Polygon,pts:List[Polygon]):Boolean={
      val x=pts.map(p=>p.intersects(py))
      val b=x.exists(a=>a==true)
      return b
    }
    def wallIntersect(py1:Polygon,py2:Polygon):Boolean={
      py1.intersects(py2)
    }
    def sameBuild(w1:Wall,w2:Wall):Boolean={
      
      w1.building==w2.building}
     
    
    def setConstraints(walls:List[Wall]):Unit={
      walls match  {
        case Nil => 
        case x::Nil=>x.inner.addConstraint(ins=>if(intersectList(ins,outsidePt)) 0.01;else 0.99 )
        case x::xs=>setConstraint(x,xs);println("setting")
                    setConstraints(xs)
        case _=>  
      }
    }
    
    def sameBuildCloseConstraint(w1:Wall,w2:Wall){
      val b1=w1.building
      val b2=w2.building
      val pair= ^^(b1,b2)
      val dist= w1.lineStr.distance(w2.lineStr)+.00001
      println(dist)
      val p= math.min(1/(30000*dist),1.0)
      println(p)
      pair.addConstraint(pr=>if(pr._1==pr._2) p; else 0.1)
    }
    
    def setConstraint(x:Wall,xs:List[Wall]){
       for(newWall<-xs){
         sameBuildCloseConstraint(x,newWall)
          val inr=x.inner
          val otr=newWall.inner
          inr.addConstraint(ins=>if(intersectList(ins,outsidePt)) 0.02;else 0.98 )
          val pair=  ^^(inr,otr)
          pair.addConstraint(ins=>if(wallIntersect(ins._1,ins._2) & sameBuild(x,newWall)) 0.98;
            //else if(wallIntersect(ins._1,ins._2)==false & sameBuild(x,newWall)==false) 0.49;
            else .02)
//          pair.addConstraint(ins=>if(sameBuild(x,newWall)) 0.49;
//            else if(!wallIntersect(ins._1,ins._2) & !sameBuild(x,newWall)) 0.49;
//            else .02)  
       }
    }
 
}
  
  
  
def  reason:List[(Polygon,String)]={
  this.constrain // MPEVariableElimination() 
      val mpeMH =MetropolisHastingsAnnealer(chooseScheme, Schedule.default(1.0))
        mpeMH.start()
        Thread.sleep(30000)
    val wallIns=unassignedWalls.map(w=>w.inner)
    val build:List[Element[String]]=unassignedWalls.map(w=>w.building)
    val exp2:List[Polygon]= wallIns.map(x=>mpeMH.mostLikelyValue(x  ))
    val exp1:List[String]= build.map(x=>mpeMH.mostLikelyValue(x  ))
    mpeMH.kill()
    println("alg finished")
    println(exp2)
    println(exp1)
   // mpeMH.kill()
    return exp2 zip exp1
  }

def assignBuildings={
   val preBuilding=reason
   val (insides,nums)=preBuilding.unzip
   val names=nums.distinct
   //val buildings=names.map { x => new Building(x) }
   for(n<-names)yield{
     val b=new Building(n)
     val sList=preBuilding.filter(p=>p._2==n)
     b.wallInsides=sList.map(x=>x._1)
     
   }
  
}


def assignInside:List[Polygon]={  
    obs.filter(x=>x._2=="Inside").map(y=>y._1)
}

def assignOutside:List[Polygon]={  
    obs.filter(x=>x._2=="Outside").map(y=>y._1)
    }
  
//constraints  
  
  

//  def assignOrient(bs:List[Boolean]){val pairs=unassignedWalls.zip(bs)
//        pairs.foreach(x=>x._1.orientation=x._2)  
//  }
//  def assignExtent(ds:List[Double]){val pairs=unassignedWalls.zip(ds)
//        pairs.foreach(x=>x._1.extent=x._2)  
//  } 
//  def assignOrientExtent(ds:List[Double],bs:List[Boolean]):Neighborhood={
//      val nwN=this
//      nwN.assignOrient(bs)
//      nwN.assignExtent(ds)
//      return nwN
//  }
  def assignWalls{
      unassignedWalls=walls.map { x => new Wall(x) } 
  }
  private def chooseScheme():ProposalScheme={
    val random = new scala.util.Random()
    val numWall=unassignedWalls.length
    DisjointScheme(
     (.2, ()=>ProposalScheme(unassignedWalls(random.nextInt(numWall)).extent)),
     (.2, ()=> ProposalScheme(unassignedWalls(random.nextInt(numWall)).orientation) ),
     (.2, ()=> ProposalScheme(unassignedWalls(random.nextInt(numWall)).building) ),
     (.2, ()=> ProposalScheme(unassignedWalls(random.nextInt(numWall)).orientation,unassignedWalls(random.nextInt(numWall)).orientation)),
     (.2,()=>ProposalScheme(unassignedWalls(random.nextInt(numWall)).extent,unassignedWalls(random.nextInt(numWall)).extent)))
//    def changeDist()=ProposalScheme(unassignedWalls(random.nextInt(numWall)).extent)
//    def changeOrient()=ProposalScheme(unassignedWalls(random.nextInt(numWall)).orientation)
//    def changeBuilding()=ProposalScheme(unassignedWalls(random.nextInt(numWall)).building)
      //def change2Orient() =ProposalScheme(unassignedWalls(random.nextInt(numWall)).orientation,unassignedWalls(random.nextInt(numWall)).orientation)  
      //def change2Dist() =ProposalScheme(unassignedWalls(random.nextInt(numWall)).extent,unassignedWalls(random.nextInt(numWall)).extent)
  }

  
  
  
  //////helper functions for loading walls and points and saving insides and walls and obs
  
  
//  def returnInsides:List[Polygon]={
//      val p= unassignedWalls.map { w => w.innerSide }
//      println(p)
//      return p
//  }
  
  //loadPTS           
   
val ptBuilder:SimpleFeatureType=DataUtilities.createType("Observation","the_geom:Point:srid=4326,"+"dataType:String")//+"id:Int")
val lnBuilder:SimpleFeatureType=DataUtilities.createType("Location","the_geom:LineString:srid=4326," +"dataType:String")
 
def loadObsPts(shapeName:String){     
    val file:File= new File(shapeName)
    val map:JMap[String,JSerial] =new JHashMap()
    map.put("url",file.toURI().toURL().asInstanceOf[JSerial])
    val dataStore:DataStore=DataStoreFinder.getDataStore(map )
     //:SimpleFeatureSource 
    val typeNameSS=dataStore.getTypeNames()(0)
  //  println(typeNameSS)
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
        //  println( feature.getAttribute("the_geom") )
       //   println( feature.getAttribute("dataType"))
          val d=feature.getAttribute("dataType").asInstanceOf[String]
          val x=feature.getAttribute("the_geom").asInstanceOf[Point]
          ptList=ptList:+x
          typeList=typeList:+d
     }
 }
 finally {
 //  println(ptList)
 //  println(typeList)
     iterator.close();
 }
         obs=ptList zip typeList
  } 
///load walls         
  def loadWalls(shapeName:String ){
    val file:File= new File(shapeName)
    val map:JMap[String,JSerial] =new JHashMap()
    map.put("url",file.toURI().toURL().asInstanceOf[JSerial])
    val dataStore:DataStore=DataStoreFinder.getDataStore(map )
     //:SimpleFeatureSource 
    val typeNameSS=dataStore.getTypeNames()(0)
   // println(typeNameSS)
    val typeName:String=dataStore.getTypeNames()(0)
    val featureSource= dataStore.getFeatureSource("Walls" );
    val featureCollection:SimpleFeatureCollection = featureSource.getFeatures();
    //val dat=factory.createDataStore(file.toURI().toURL())
     val  iterator:SimpleFeatureIterator = featureCollection.features();
     var lnList:List[LineString]=List()
      
 try {
     while( iterator.hasNext() ){
           val feature: SimpleFeature = iterator.next();
       //   println( feature.getAttribute("the_geom") )
         
           
          val x=feature.getAttribute("the_geom").asInstanceOf[MultiLineString]
          val L=x.getLength
       //   println(L)
          val z=for(n <- 0 to L.toInt) yield x.getGeometryN(n).asInstanceOf[LineString]
        //  println(z)
          lnList=lnList++z.toList
          
           
     }
 }
 finally {
   //println(lnList)
 
     iterator.close();
 }
    walls=lnList
   // println(walls)
  }

  
}

