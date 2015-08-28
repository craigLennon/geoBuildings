package Input
import java.awt.Color;
import java.awt.event.ActionEvent;
import java.io.{File,Serializable=>JSerial}
import java.io.IOException
import scala.throws
import java.util.ArrayList;
import java.util.{List=>jList}
import javax.swing.{ButtonGroup,JRadioButtonMenuItem,AbstractButton,JToolBar,JButton,JDialog }
import org.geotools.geometry._
import java.awt.geom.Point2D
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JOptionPane;
import org.geotools.swing.event.MapMouseEvent;
import java.awt.event.ActionEvent;
import java.awt.event.{ComponentListener,ComponentEvent,ActionListener}     
import com.vividsolutions.jts.geom._
import org.geotools.coverage.GridSampleDimension;
import org.geotools.coverage.grid.GridCoverage2D;
import org.geotools.coverage.grid.io.AbstractGridFormat;
import org.geotools.coverage.grid.io.GridCoverage2DReader;
import org.geotools.coverage.grid.io.GridFormatFinder;
import org.geotools.data.FileDataStore;
import org.geotools.data.FileDataStoreFinder;
import org.geotools.data.Parameter;
import org.geotools.data.simple.SimpleFeatureSource;
import org.geotools.factory.CommonFactoryFinder;
import org.geotools.map.FeatureLayer;
import org.geotools.map.GridReaderLayer;
import org.geotools.map.Layer;
import org.geotools.map.MapContent;
import org.geotools.map.StyleLayer;
import org.geotools.styling.ChannelSelection;
import org.geotools.styling.ContrastEnhancement;
import org.geotools.styling.RasterSymbolizer;
import org.geotools.styling.SLD;
import org.geotools.styling.SelectedChannelType;
import org.geotools.styling.Style;
import org.geotools.styling.StyleFactory;
import org.geotools.swing.JMapFrame;
import org.geotools.swing.action.SafeAction;
import org.geotools.swing.data.JParameterListWizard;
import org.geotools.swing.wizard.JWizard;
import org.geotools.swing.tool.CursorTool
import org.geotools.util.KVP;
import org.opengis.filter.FilterFactory2;
import org.opengis.style.ContrastMethod;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.{Map=>JMap};

import javax.swing.UIManager;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;

import org.geotools.data.DataUtilities;
import org.geotools.data.DefaultTransaction;
import org.geotools.data.Transaction;
import org.geotools.data.collection.ListFeatureCollection;
import org.geotools.data.shapefile.ShapefileDataStore;
import org.geotools.data.shapefile.ShapefileDataStoreFactory;
import org.geotools.data.simple.SimpleFeatureCollection;
import org.geotools.data.simple.SimpleFeatureSource;
import org.geotools.data.simple.SimpleFeatureStore;
import org.geotools.feature.simple.SimpleFeatureBuilder;
import org.geotools.feature.simple.SimpleFeatureTypeBuilder;
import org.geotools.geometry.jts.JTSFactoryFinder;
import org.geotools.referencing.crs.DefaultGeographicCRS;
import org.geotools.swing.data.JFileDataStoreChooser;
import org.opengis.feature.simple.SimpleFeature;
import org.opengis.feature.simple.SimpleFeatureType;
/**
 * @author cLennon
 */
class MapInput {
      private val sf = CommonFactoryFinder.getStyleFactory();
      private val ff = CommonFactoryFinder.getFilterFactory2();
      val frame= new JMapFrame 

        
      def execute{
       getLayersAndDisplay()
      }
      
     var rasterName:String=""  //record of file names for later display
     var shapeObsName:String="" 
     var shapeWallName:String=""
     
        def getLayersAndDisplay(){
        val list:jList[Parameter[_]]=new ArrayList[Parameter[_]]
        list.add((new Parameter("image", classOf[File], "Image",
                "GeoTiff or World+Image to display as basemap",
                new KVP( Parameter.EXT, "tif", Parameter.EXT, "jpg"))))
        val wizard:JParameterListWizard = new JParameterListWizard("Image Lab","Fill in the following layers", list)
        val finish = wizard.showModalDialog()                
        if (finish != JWizard.FINISH) {
            System.exit(0);
        }
        val imageFile:File =  wizard.getConnectionParameters().get("image").asInstanceOf[File]
        val  shapeList=List()
        rasterName=imageFile.getAbsolutePath() 
        displayLayers(imageFile, shapeList);
      } //end of get layers and display
      
      
  def displayLayers( rasterFile:File,shpList:List[File]) {
        val format:AbstractGridFormat  = GridFormatFinder.findFormat( rasterFile );        
        val reader = format.getReader(rasterFile);
        //helper functions
    
     def createGreyscaleStyle0(band:Int):Style= {
        val ce:ContrastEnhancement = sf.contrastEnhancement(ff.literal(1.0), ContrastMethod.NORMALIZE);
        val sct:SelectedChannelType = sf.createSelectedChannelType(String.valueOf(band), ce);
        val sym:RasterSymbolizer = sf.getDefaultRasterSymbolizer();
        val sel:ChannelSelection = sf.channelSelection(sct);
        sym.setChannelSelection(sel);
        return SLD.wrapSymbolizers(sym);
    } //end create grey scale style
     
     def createRGBStyle():Style= {
        var cov:GridCoverage2D = null
        try {
            cov = reader.read(null);
        } catch{
          case ex: IOException=> throw new RuntimeException
        }
        // We need at least three bands to create an RGB style
        val numBands = cov.getNumSampleDimensions();
        if (numBands < 3) {
            return null;
        } // Get the names of the bands
        val sampleDimensionNames = new  Array[String](numBands);
        var ind=0
        while(ind<numBands){
            val dim:GridSampleDimension = cov.getSampleDimension(ind);
            sampleDimensionNames(ind) = dim.getDescription().toString();          
            ind+=1
        }
          val RED = 0 
          val GREEN = 1
          val BLUE = 2
        val channelNum = Array( -1, -1, -1 )
        ind=0
        while(ind<numBands){   
            val name = sampleDimensionNames(ind).toLowerCase();
            if (name != null) {
                if (name.matches("red.*")) {
                    channelNum(RED) = ind + 1;
                } else if (name.matches("green.*")) {
                    channelNum(GREEN) = ind + 1;
                } else if (name.matches("blue.*")) {
                    channelNum(BLUE) = ind + 1;
                }
            }
            ind+=1
        }
        // If we didn't find named bands "red...", "green...", "blue..."
        // we fall back to using the first three bands in order
        if (channelNum(RED) < 0 || channelNum(GREEN) < 0 || channelNum(BLUE) < 0) {
            channelNum(RED) = 1;
            channelNum(GREEN) = 2;
            channelNum(BLUE)= 3;
        }
        // Now we create a RasterSymbolizer using the selected channels
        val len=cov.getNumSampleDimensions()
      
        val sct:Array[SelectedChannelType] = new Array[SelectedChannelType](len)
        val ce:ContrastEnhancement = sf.contrastEnhancement(ff.literal(1.0), ContrastMethod.NORMALIZE);
                ind=0
        while(ind<3){
            sct(ind) = sf.createSelectedChannelType(String.valueOf(channelNum(ind)), ce);
            ind+=1
        }
        val sym:RasterSymbolizer = sf.getDefaultRasterSymbolizer();
        val sel:ChannelSelection = sf.channelSelection(sct(RED), sct(GREEN), sct(BLUE));
        sym.setChannelSelection(sel);

        return SLD.wrapSymbolizers(sym);
    }  //end of create rbg style

     
        // Initially display the raster in greyscale using the data from the first image band
        val rasterStyle:Style = createGreyscaleStyle0(1);

        // Set up a MapContent with the two layers
        val map:MapContent = new MapContent();
        map.setTitle("ImageLab");

       // var pts:List[List[DirectPosition2D]]=List()
        val rasterLayer:Layer = new GridReaderLayer(reader, rasterStyle);
        map.addLayer(rasterLayer);
        println(rasterLayer.getBounds)

        val frame = new JMapFrame(map);

        class curse extends CursorTool{  
        }
        
       val ptBuilder:SimpleFeatureType=DataUtilities.createType("Observation",
            "the_geom:Point:srid=4326,"+"dataType:String")//+"id:Int")
        val lnBuilder:SimpleFeatureType=DataUtilities.createType("Location",
            "the_geom:LineString:srid=4326," +"dataType:String")
        val newPT:DirectPosition2D=new DirectPosition2D()
        val pointBuilder :SimpleFeatureBuilder=new SimpleFeatureBuilder(ptBuilder)
              
        val lineBuilder :SimpleFeatureBuilder=new SimpleFeatureBuilder(lnBuilder)
        val geometryFactory:GeometryFactory = JTSFactoryFinder.getGeometryFactory()
        var obsType="none"
        var ObservationPoints:List[SimpleFeature]=List()
        var Walls:List[SimpleFeature]=List()
        //functions for adding walls and observation points
        def addWall(pts:List[DirectPosition2D],dataType:String,lnBuilder:SimpleFeatureBuilder):SimpleFeature={
         println("in add wall")
           println(pts)   
           def mkPT(xy:DirectPosition2D):Coordinate={new Coordinate(xy.getX, xy.getY)}
              val ac=pts.map(x=>mkPT(x))
              val ls =geometryFactory.createLineString(ac.toArray)
              println("inestring done")
              lnBuilder.add(ls)
              lnBuilder.add(dataType)
              val feature:SimpleFeature=lnBuilder.buildFeature(null)
              return feature              
       }
        
        def addObs(newPt:DirectPosition2D,dataType:String,pointBuilder:SimpleFeatureBuilder):SimpleFeature={
          println(newPt)
          val point:Point=geometryFactory.createPoint(new Coordinate(newPt.getX, newPt.getY))
          pointBuilder.add(point)
          pointBuilder.add(dataType)
          val feature:SimpleFeature=pointBuilder.buildFeature(null)
          return feature
        } 

 //building the interface

//        frame.getMapPane().setRenderer(s)
//establishing how data is recorded     

        frame.getMapPane().setCursorTool(
              new curse {
              
                var newPt:List[DirectPosition2D]=List()
              override  def onMouseClicked(ev:MapMouseEvent){
                  
                  println(obsType)
                  obsType match{
                    case "none"=>println("no type selected")
                    case "Inside"=>  ObservationPoints=ObservationPoints:+addObs(ev.getWorldPos() ,obsType,pointBuilder)
                    case "Outside"=>ObservationPoints=ObservationPoints:+addObs(ev.getWorldPos() ,obsType,pointBuilder)
                    case "Wall"  =>//newPt=List(ev.getWorldPos())
                    //println(newPt)
                    case _ =>println("type error")
                  }                     
                }

              override  def onMousePressed(ev:MapMouseEvent){
                  
                  println(obsType)
                  obsType match{
                    case "none"=>println("no type selected")
                    case "Inside"=>//ObservationPoints=ObservationPoints:+addObs(ev.getWorldPos() ,obsType,pointBuilder)
                    case "Outside"=>//ObservationPoints=ObservationPoints:+addObs(ev.getWorldPos() ,obsType,pointBuilder)
                    case "Wall"  =>newPt=List(ev.getWorldPos())
                    println(newPt)
                    case _ =>println("type error")
                  }                     
                }
                
                
              override def onMouseReleased(ev:MapMouseEvent){
                obsType match{
                    case "none"=>println("no type selected")
                    case "Inside"=>()
                    case "Outside"=>()
                    case "Wall"  =>
                      println("release")
                      newPt=newPt:+ev.getWorldPos()
                      println
                      println(newPt)
                      Walls=Walls:+addWall(newPt,obsType,lineBuilder)
                    case _ =>println("type error")
                  }
              }})
 ///data entry is established.  now we build the rest or the frame
        frame.setSize(1600, 900);
        frame.enableStatusBar(true);
        //frame.enableTool(JMapFrame.Tool.POINTER , JMapFrame.Tool.ZOOM, JMapFrame.Tool.PAN, JMapFrame.Tool.RESET);
        frame.enableToolBar(true);
        val toolbar: JToolBar=frame.getToolBar
        val btnC:JButton = new JButton("Calculate")
        toolbar.addSeparator();
        toolbar.add(btnC)
        btnC.addActionListener(new ActionListener(){
                  def actionPerformed(e:ActionEvent){
                  println("calculating")
                  val n = new interpret.Neighborhood
                  n.loadObsPts(shapeObsName)
                  n.loadWalls(shapeWallName)
  }
})


      def saveShape(features:List[SimpleFeature],name:String,builder:SimpleFeatureType){
//        val chooser:JFileDataStoreChooser = new JFileDataStoreChooser("shp");
//        chooser.setDialogTitle("Save "+name +" shapefile");
//        chooser.setSelectedFile(new File(name+".shp"));
//        val returnVal= chooser.showSaveDialog(null)

        val newFile:File  = new File("C:/Users/cLennon/Desktop/wallExp/"+name+".shp")  //chooser.getSelectedFile()
        println("in saved")
        val dataStoreFactory:ShapefileDataStoreFactory = new ShapefileDataStoreFactory();

        val  params:JMap[String, JSerial] = new HashMap[String, JSerial]
        params.put("url", newFile.toURI().toURL());
        params.put("create spatial index", true);
        
        
        def saveFeature(params:JMap[String, JSerial],lnBuilder:SimpleFeatureType,
            flist:List[SimpleFeature]){
           val transaction:Transaction = new DefaultTransaction();
           val newDataStore:ShapefileDataStore = dataStoreFactory.
           createDataStore(params).asInstanceOf[ShapefileDataStore]
            newDataStore.createSchema(lnBuilder)
            val typeName:String = newDataStore.getTypeNames()(0)
            val featureSource:SimpleFeatureSource = newDataStore.getFeatureSource(typeName)
            val SHAPE_TYPE:SimpleFeatureType = featureSource.getSchema()
                    if(featureSource.isInstanceOf[SimpleFeatureStore]) {
             val featureStore:SimpleFeatureStore = featureSource.asInstanceOf[SimpleFeatureStore]
            /*
             * SimpleFeatureStore has a method to add features from a
             * SimpleFeatureCollection object, so we use the ListFeatureCollection
             * class to wrap our list of features.
             */
             val collection:SimpleFeatureCollection = new ListFeatureCollection(SHAPE_TYPE, flist.toArray);
            featureStore.setTransaction(transaction);
            try {
                featureStore.addFeatures(collection);
                transaction.commit();
            } 
            //catch (problem Exception) {
              //  problem.printStackTrace();
                //transaction.rollback();
             finally {
                transaction.close();
            }
         //   System.exit(0); // success!
        } else {
            System.out.println(typeName + " does not support read/write access");
         //   System.exit(1);
        }
    }
        ////here we are.  It saved one point.  did the rest not save, or were they not recorded?
        //also we need to test saving several to the same file
        saveFeature(params,builder,features)
     
     }
     

        
      val btnS:JButton = new JButton("Save Input")
        toolbar.addSeparator();
        toolbar.add(btnS)
        btnS.addActionListener(new ActionListener(){
                  def actionPerformed(e:ActionEvent){
                    println("saved")
                    saveShape(ObservationPoints,"InOutPts",ptBuilder)
                    saveShape(Walls,"Walls",lnBuilder)
                     shapeObsName="C:/Users/cLennon/Desktop/wallExp/"+"InOutPts"+".shp"
                     shapeWallName="C:/Users/cLennon/Desktop/wallExp/"+"Walls"+".shp"
        val m = new MapUpdate(rasterName,shapeObsName,shapeWallName)
                    m.execute
                  }
        })       
        
        frame.enableLayerTable(true)
        val menuBar:JMenuBar = new JMenuBar();
        frame.setJMenuBar(menuBar);
        val typeMenu:JMenu = new JMenu("Observation Type")
        
//a group of radio button menu items
  
        
        val group:ButtonGroup = new ButtonGroup();
        val rbMenuItem1 = new JRadioButtonMenuItem("Wall");
            rbMenuItem1.setSelected(true);
            group.add(rbMenuItem1);
            typeMenu.add(rbMenuItem1);

        val rbMenuItem2 = new JRadioButtonMenuItem("Inside");
            rbMenuItem2.setSelected(false);
            group.add(rbMenuItem2);
            typeMenu.add(rbMenuItem2);
            
        val rbMenuItem3 = new JRadioButtonMenuItem("Outside");
            rbMenuItem3.setSelected(false);
            group.add(rbMenuItem3);
            typeMenu.add(rbMenuItem3);
            
     val sliceActionListener: ActionListener = new ActionListener() {
      def actionPerformed( actionEvent:ActionEvent){
         val aButton:AbstractButton =  actionEvent.getSource().asInstanceOf[AbstractButton]
         obsType=aButton.getText()
        println("Selected: " +" " +obsType);
      }
    }
        rbMenuItem1.addActionListener(sliceActionListener)
        rbMenuItem2.addActionListener(sliceActionListener)
        rbMenuItem3.addActionListener(sliceActionListener)
        
        val menu:JMenu = new JMenu("Raster");

        
        menu.add( new SafeAction("Grayscale display") {
            def action(e:ActionEvent){
                val style:Style = createGreyscaleStyle();
                if (style != null) {
                   map.layers().get(0).asInstanceOf[StyleLayer].setStyle(style);
                    frame.repaint();
                }
            }
        });
       menu.add( new SafeAction("RGB display") {
            def action(e:ActionEvent){
                val style:Style = createRGBStyle();
                if (style != null) {
                    map.layers().get(0).asInstanceOf[StyleLayer].setStyle(style);
                    frame.repaint();
                }
           }
        })
        
        //add menus
        menuBar.add(menu);
        menuBar.add(typeMenu)
        frame.setVisible(true);
      //style function
        
 
        
        
      def  createGreyscaleStyle():Style= {
        var cov:GridCoverage2D = null
        try {
            cov = reader.read(null);
        } catch{
          case ex: IOException=> throw new RuntimeException
        }
        val numBands = cov.getNumSampleDimensions();
        val bandNumbers = (1 to numBands).toList.toArray.asInstanceOf[Array[Object]]
        val selection:Object = JOptionPane.showInputDialog(
                frame,
                "Band to use for greyscale display",
                "Select an image band",
                JOptionPane.QUESTION_MESSAGE,
                null,
                bandNumbers,
                1);
        if (selection != null) {
            val band = (selection.asInstanceOf[Number]).intValue();
            return createGreyscaleStyle0(band);
        }
        return null;
    }
        
    }//end display
     
}

////class for using wms
//import java.net.URL;
//import java.util.{List=>JList,Arrays};
//
//import javax.swing.JFrame;
//import javax.swing.JOptionPane;
//import org.geotools.data.wms._
//import org.geotools.data.ows.{Layer=>wsLayer};
//import org.geotools.data.wms.WebMapServer;
//import org.geotools.map.MapContent;
//import org.geotools.map.WMSLayer;
//import org.geotools.swing.JMapFrame;
//import org.geotools.swing.wms.WMSChooser;
//import org.geotools.swing.wms.WMSLayerChooser;
//
///**
// * This is a Web Map Server "quickstart" doing the minimum required to display
// * something on screen.
// */
// class WMSLab extends JFrame {
//    /**
//     * Prompts the user for a wms service, connects, and asks for a layer and then
//     * and displays its contents on the screen in a map frame.
//     */
//    
//        // display a data store file chooser dialog for shapefiles
//        def init{
//        val server:JList[String]=Arrays.asList(
////            "http://services.nationalmap.gov/arcgis/services/USGSTopoLarge/MapServer/WMSServer?request=GetCapabilities&service=WMS",
////            "http://services.nationalmap.gov/arcgis/services/structures/MapServer/WMSServer?request=GetCapabilities&service=WMS",
////            "http://services.nationalmap.gov/arcgis/services/transportation/MapServer/WMSServer?request=GetCapabilities&service=WMS",
////            "http://raster.nationalmap.gov/arcgis/services/Orthoimagery/USGS_EROS_Ortho_NAIP/ImageServer/WMSServer?request=GetCapabilities&service=WMS"
//        "http://webservices.nationalatlas.gov/wms/1million?SERVICE=WMS&REQUEST=GetCapabilities"  , 
// "http://webservices.nationalatlas.gov/wms/agriculture?SERVICE=WMS&REQUEST=GetCapabilities",   
//   "http://webservices.nationalatlas.gov/wms/biology?SERVICE=WMS&REQUEST=GetCapabilities",  
// "http://webservices.nationalatlas.gov/wms/boundaries?SERVICE=WMS&REQUEST=GetCapabilities",   
//   "http://webservices.nationalatlas.gov/wms/climate?SERVICE=WMS&REQUEST=GetCapabilities", 
//"http://webservices.nationalatlas.gov/wms/environment?SERVICE=WMS&REQUEST=GetCapabilities",  
//"http://webservices.nationalatlas.gov/wms/geology?SERVICE=WMS&REQUEST=GetCapabilities",   
//"http://webservices.nationalatlas.gov/wms/government?SERVICE=WMS&REQUEST=GetCapabilities",   
//"http://webservices.nationalatlas.gov/wms/history?SERVICE=WMS&REQUEST=GetCapabilities" , 
//"http://webservices.nationalatlas.gov/wms/map_reference?SERVICE=WMS&REQUEST=GetCapabilities",  
//"http://webservices.nationalatlas.gov/wms/people?SERVICE=WMS&REQUEST=GetCapabilities"  ,
//"http://webservices.nationalatlas.gov/wms/transportation?SERVICE=WMS&REQUEST=GetCapabilities",   
//"http://webservices.nationalatlas.gov/wms/water?SERVICE=WMS&REQUEST=GetCapabilities"   ,
//"http://webservices.nationalatlas.gov/wms?SERVICE=WMS&REQUEST=GetCapabilities"
//        )  
//        val capabilitiesURL:URL = WMSChooser.showChooseWMS(
//            server);
//        if( capabilitiesURL == null ){
//          println("err1")
//            System.exit(0); // canceled
//        }
//        
////       val testURL:URL=new URL("http://raster.nationalmap.gov/arcgis/services/Orthoimagery/USGS_EROS_Ortho_NAIP/ImageServer/WMSServer?request=GetCapabilities&service=WMS")
////       val wms0=new WMS1_3_0.GetCapsRequest(testURL)
////       val x=wms0.getFinalURL
////       val wms:WebMapServer = new WebMapServer( x );        
//        println("got capability")
//        println(capabilitiesURL)
//       val wms:WebMapServer = new WebMapServer( capabilitiesURL );
//        println("got webserver")
//        val wmsLayers:JList[wsLayer] = WMSLayerChooser.showSelectLayer( wms );
//        println("got layers")
//        if( wmsLayers == null ){
//          println("err2")
//            JOptionPane.showMessageDialog(null, "Could not connect - check url");
//            System.exit(0);
//        }
//        val mapcontent:MapContent = new MapContent();
//        mapcontent.setTitle( wms.getCapabilities().getService().getTitle() );
//        println("map in")
//        for( wmsLayer <-wmsLayers.toArray()  ){
//            val displayLayer:WMSLayer = new WMSLayer(wms, wmsLayer.asInstanceOf[wsLayer] );
//            mapcontent.addLayer(displayLayer);
//            println("map added")
//        }
//        println("diplay")
//        // Now display the map
//        JMapFrame.showMap(mapcontent);
//        println("display done")
//        }
//}
