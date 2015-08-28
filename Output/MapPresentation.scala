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
        list.add(new Parameter("shape1", classOf[File], "Shapefile",
                "Shapefile contents to display", new KVP(Parameter.EXT, "shp"))) 
        list.add(new Parameter("shape2", classOf[File], "Shapefile",
                "Shapefile contents to display", new KVP(Parameter.EXT, "shp"))) 
                
        val wizard:JParameterListWizard = new JParameterListWizard("Image Lab","Fill in the following layers", list)
        val finish = wizard.showModalDialog()
                
        if (finish != JWizard.FINISH) {
            System.exit(0);
        }
        val imageFile:File =  wizard.getConnectionParameters().get("image").asInstanceOf[File]
        val shapeFile1:File =  wizard.getConnectionParameters().get("shape1").asInstanceOf[File]
        val shapeFile2:File =  wizard.getConnectionParameters().get("shape2").asInstanceOf[File] 
              
        val  shapeList=List(shapeFile1,shapeFile2)

       
        
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
      
        import javax.swing.{ButtonGroup,JRadioButtonMenuItem,AbstractButton,JToolBar,JButton }
        frame.setSize(800, 600);
        frame.enableStatusBar(true);
        frame.enableTool(JMapFrame.Tool.POINTER , JMapFrame.Tool.ZOOM, JMapFrame.Tool.PAN, JMapFrame.Tool.RESET);
        frame.enableToolBar(true);

        
        
        frame.enableLayerTable(true)
        val menuBar:JMenuBar = new JMenuBar();
        frame.setJMenuBar(menuBar);
       
        
//a group of radio button menu items
       
        
      
   
      
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

