package Output
import java.awt.Color;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException
import scala.throws
import java.util.ArrayList;
import java.util.{List=>jList}

import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JOptionPane;

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
class MapPresentation {
      private val sf = CommonFactoryFinder.getStyleFactory();
      private val ff = CommonFactoryFinder.getFilterFactory2();
      val frame= new JMapFrame 

        
      def execute{
       this.getLayersAndDisplay()
      }
      
    
      
      private def getLayersAndDisplay(){
        val list:jList[Parameter[_]]=new ArrayList[Parameter[_]]
        list.add((new Parameter("image", classOf[File], "Image",
                "GeoTiff or World+Image to display as basemap",
                new KVP( Parameter.EXT, "tif", Parameter.EXT, "jpg"))))
      //  list.add(new Parameter("shape1", classOf[File], "Shapefile",
       //         "Shapefile contents to display", new KVP(Parameter.EXT, "shp"))) 
     //   list.add(new Parameter("shape2", classOf[File], "Shapefile",
      //          "Shapefile contents to display", new KVP(Parameter.EXT, "shp"))) 
                
        val wizard:JParameterListWizard = new JParameterListWizard("Image Lab","Fill in the following layers", list)
        val finish = wizard.showModalDialog()
                
        if (finish != JWizard.FINISH) {
            System.exit(0);
        }
        val imageFile:File =  wizard.getConnectionParameters().get("image").asInstanceOf[File]
     //   val shapeFile1:File =  wizard.getConnectionParameters().get("shape1").asInstanceOf[File]
       // val shapeFile2:File =  wizard.getConnectionParameters().get("shape2").asInstanceOf[File] 
              
     //   val  shapeList=List(shapeFile1,shapeFile2)
 val  shapeList=List()
       
        
        displayLayers(imageFile, shapeList);
        
      }
      
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
    }
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
        }
        // Get the names of the bands
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
        
        // We examine the band names looking for "red...", "green...", "blue...".
        // Note that the channel numbers we record are indexed from 1, not 0.
       
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
    }
        
        //end of helpers

        // Initially display the raster in greyscale using the
        // data from the first image band
        val rasterStyle:Style = createGreyscaleStyle0(1);

        // Connect to the shapefile
        val dataStoreList:List[FileDataStore] = shpList.map(x=> FileDataStoreFinder.getDataStore(x))
        val shapefileSourceList:List[SimpleFeatureSource] =dataStoreList.map(dat=> dat.getFeatureSource())

        // Create a basic style with yellow lines and no fill
        val shpStyle:Style = SLD.createPolygonStyle(Color.YELLOW, null, 0.0f);

        // Set up a MapContent with the two layers
        val map:MapContent = new MapContent();
        map.setTitle("ImageLab");
        import org.geotools.geometry._
        import java.awt.geom.Point2D
        var pts:List[List[DirectPosition2D]]=List()
        val rasterLayer:Layer = new GridReaderLayer(reader, rasterStyle);
        map.addLayer(rasterLayer);
        println(rasterLayer.getBounds)
        for(shapefileSource<- shapefileSourceList){
        val shpLayer:Layer = new FeatureLayer(shapefileSource, shpStyle);
        map.addLayer(shpLayer);
        println(shpLayer.getBounds)}
        // Create a JMapFrame with a menu to choose the display style for the
        val frame = new JMapFrame(map);
        import org.geotools.swing.event.MapMouseEvent;
        import java.awt.event.ActionEvent;
        import java.awt.event.{ComponentListener,ComponentEvent,ActionListener}
       
        import com.vividsolutions.jts.geom._
        class curse extends CursorTool{  
        }
        
                val ptBuilder:SimpleFeatureType=DataUtilities.createType("Observation",
            "the_geom:Point:srid=4326,"+"dataType:String")//+"id:Int")
        //val lnBuilder:SimpleFeatureType=DataUtilities.createType("Location",
          //  "the_geom:LineString:srid=4326","type:String")
        val newPT:DirectPosition2D=new DirectPosition2D()
        val pointBuilder :SimpleFeatureBuilder=new SimpleFeatureBuilder(ptBuilder)
              
        //val lineBuilder :SimpleFeatureBuilder=new SimpleFeatureBuilder(lnBuilder)
        val geometryFactory:GeometryFactory = JTSFactoryFinder.getGeometryFactory()
       
        def addObs(newPt:DirectPosition2D,dataType:String):SimpleFeature={
          val point:Point=geometryFactory.createPoint(new Coordinate(newPt.getX, newPt.getY))
          pointBuilder.add(point)
          pointBuilder.add(dataType)
          val feature:SimpleFeature=pointBuilder.buildFeature(null)
          return feature
        }
        
        
        var feaList:List[SimpleFeature]=List()
        frame.getMapPane().setCursorTool(
              new curse {
                var wall=false
                var newPt:List[DirectPosition2D]=List()
              override  def onMouseClicked(ev:MapMouseEvent){
                  wall = false
                  println("Mouse click at: " + ev.getWorldPos())
                        newPt=List(ev.getWorldPos())
                      
                  val fea=addObs(ev.getWorldPos() ,"noneYet")   
                  feaList=feaList:+fea
                      println(pts)
                      println(fea)
                      
                }
       
              override def onMouseDragged(ev:MapMouseEvent){
                wall = true
              }
              
              override def onMouseReleased(ev:MapMouseEvent){
                      if(wall){newPt=newPt:+ ev.getWorldPos()
                      pts=pts:+newPt
                      println(pts)}
              }
              }
        )
        /////building simple features
        //you are here trying to build points and lines from input
        //////////////////////////
        
        //insert button finish.  When pressed write to file then estimate
        
        

        
        
//        frame.addComponentListener(new ComponentListener(){}
//              onMouseClicked    
//        )
//        val cur=new curse
        //cur.onMouseClicked(ev)
        frame.setSize(800, 600);
        frame.enableStatusBar(true);
        frame.enableTool(JMapFrame.Tool.POINTER , JMapFrame.Tool.ZOOM, JMapFrame.Tool.PAN, JMapFrame.Tool.RESET);
        frame.enableToolBar(true);
        frame.enableLayerTable(true)
        val menuBar:JMenuBar = new JMenuBar();
        frame.setJMenuBar(menuBar);
                val typeMenu:JMenu = new JMenu("Observation Type")
        
//a group of radio button menu items
        var obsType="none"
        import javax.swing.{ButtonGroup,JRadioButtonMenuItem,AbstractButton }
        val group:ButtonGroup = new ButtonGroup();
        val rbMenuItem1 = new JRadioButtonMenuItem("Wall");
            rbMenuItem1.setSelected(true);
            group.add(rbMenuItem1);
            typeMenu.add(rbMenuItem1);

        val rbMenuItem2 = new JRadioButtonMenuItem("Inside");
            rbMenuItem2.setSelected(false);
            group.add(rbMenuItem2);
            typeMenu.add(rbMenuItem2);
            
        val rbMenuItem3 = new JRadioButtonMenuItem("OutSide");
            rbMenuItem3.setSelected(false);
            group.add(rbMenuItem3);
            typeMenu.add(rbMenuItem3);
            
     val sliceActionListener: ActionListener = new ActionListener() {
      def actionPerformed( actionEvent:ActionEvent){
         val aButton:AbstractButton =  actionEvent.getSource().asInstanceOf[AbstractButton]
         obsType=aButton.getText()
        println("Selected: " + aButton.getText()+" " +obsType);
      }
    }
        rbMenuItem1.addActionListener(sliceActionListener)
        rbMenuItem2.addActionListener(sliceActionListener)
        rbMenuItem3.addActionListener(sliceActionListener)
        
        val menu:JMenu = new JMenu("Raster");
        menuBar.add(menu);
        
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

            
 
        
        
        menuBar.add(typeMenu)
        
        
 
        // Finally display the map frame.
        // When it is closed the app will exit.
        frame.setVisible(true);
        
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