/////////////////////////////////////////////////////////////////////
/* Name: COLOCALIZATION ANALYSIS +	
 * Author: Katherine Herrera
 * Version: 1	Date: 13/05/2023
 * Description: This macro analyses the colocalization and JACoP 

 */ 
/////////////////////////////////////////////////////////////////////

input = getDirectory("Select folder with images");
list = getFileList(input);
output = createFolder(input, "Results");

for(i=0; i<list.length; i++){

//Open file and get data
	path = input + list[i];
	if(endsWith(path, ".tif")) {
		open(path);
		name = File.nameWithoutExtension;
	
//Change 8 bits and MAX PROJECT
		run("Set Scale...", "distance=14.1681 known=1 unit=micron global");
		run("Duplicate...", "duplicate slices=10-11"); //Number of z-stack to do a Max project.
		rename("originalz2");  
		selectWindow("originalz2");
		saveAs("Tiff" , output + name + "_originalz2" );
		run("Z Project...", "projection=[Max Intensity]");
		rename("MAX");
		setOption("ScaleConversions", true);
		run("8-bit");
		
	
	//Colocalization 
	  // Split Channels
	    selectWindow("MAX");
	    run("Split Channels");
				//C1-MAX= CD68 ; C2-MAX= 3D6 ; C3-MAX= Iba-1
				
		//PHAGOLYSOSOME
		run("Colocalization ", "channel_1=C3-MAX channel_2=C1-MAX ratio=50 threshold_channel_1=50 threshold_channel_2=50 display=255 colocalizated");
		selectWindow("Colocalizated points (8-bit) ");
		rename("phagolysosome");
		selectWindow("Colocalizated points (RGB) ");
		saveAs("Tiff" , output + name + "_phagolysosome_RGB" );
		close();
		//AB inside plaque	
		run("Colocalization ", "channel_1=C2-MAX channel_2=phagolysosome ratio=50 threshold_channel_1=50 threshold_channel_2=50 display=255 colocalizated");
		selectWindow("Colocalizated points (8-bit) ");
		rename("AB-phagolysosome");
		saveAs("Tiff" , output + name + "_AB" );
		selectWindow("Colocalizated points (RGB) ");
		saveAs("Tiff" , output + name + "_AB_RGB" );
		close();
	
		//JACoP
		run("JACoP ", "imga=C3-MAX imgb=C1-MAX thra=27 thrb=27 pearson overlap mm costesthr");   //Level of Moment threshold (27-255) for C1 and C3 channel
		selectWindow("Log");
		saveAs("Text", output + name + "_JACoP_C3+C1.txt");
		run("Close");
		
		run("JACoP ", "imga=C2-MAX imgb=phagolysosome thra=21 thrb=27 pearson overlap mm costesthr"); //For C2 channel Otsu (21-255)
		selectWindow("Log");
		saveAs("Text", output + name + "_JACoP_C2+phagolysosome.txt");
		run("Close");
		
		selectWindow("phagolysosome");
		saveAs("Tiff" , output + name + "_phagolysosome" );
		//MEASURE AND RESULTS
		
		run("Set Measurements...", "area mean min perimeter area_fraction limit display redirect=None decimal=2"); //variables 
		
		selectWindow("C1-MAX");   //CD68
		run("Measure"); 	//It's a form to know the total area of image.
		setAutoThreshold("Moments dark");
		run("Measure");
		
		selectWindow("C2-MAX");   //3D6
		setAutoThreshold("Otsu dark");
		run("Measure");
		
		selectWindow("C3-MAX");    //Iba-1
		setAutoThreshold("Moments dark");
		run("Measure");
		
		selectWindow( name + "_phagolysosome.tif" );
		setAutoThreshold("Moments dark");
		run("Measure");
				
		selectWindow( name + "_AB.tif" );
		setAutoThreshold("Moments dark");
		run("Measure");		
		
		saveAs("Results", output + name + "_AREAS.txt");
		
		//CLOSE WINDOWS
		close("*"); //cierra todas las pestañas
		run("Clear Results");
	}
}
		run("Close All");

//------------------------------
//FUNCTIONS

//Function to create a folder in the working directory
function createFolder(dir, name){
	mydir = dir+name+File.separator;
	File.makeDirectory(mydir);
	if(!File.exists(mydir)){
		exit("Unable to create the folder");
	}
	return mydir;
}