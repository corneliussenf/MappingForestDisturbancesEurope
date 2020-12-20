// Run LandTrendr and extract greatest disturbance in the Google Earth Engine (https://earthengine.google.com)
// Based on code from Justin Braaten, adapted by Cornelius Senf
// For further infos on LandTrendr and the underlying code see: https://emapr.github.io/LT-GEE/

//// Parameter definition

// A table with the outline of the study region must be imported!

// Define collection parameters
var startYear = 1983;
var endYear = 2020;
var startDay = '07-01';
var endDay = '08-30';
var country = '...'; // Muste be the same as the table name!
var aoi = ee.FeatureCollection('users/.../' + country); // Table must be imported!
var index = 'TCW'; // Chose index for segmentation
var maskThese = ['cloud', 'shadow', 'snow', 'water'];

// Define landtrendr parameters
var runParams = { 
  maxSegments:            6,
  spikeThreshold:         0.9,
  vertexCountOvershoot:   3,
  preventOneYearRecovery: true,
  recoveryThreshold:      0.25,
  pvalThreshold:          0.05,
  bestModelProportion:    0.75,
  minObservationsNeeded:  1
};

// Define change parameters

var changeParams_greatest = {
  delta:  'loss',
  sort:   'greatest',
  year:   {checked:true, start:1984, end:2020},
  mag:    {checked:false},
  dur:    {checked:false},
  preval: {checked:false},
  mmu:    {checked:false},
};


//// Rund LandTrendr and extract greatest and latest disturbance

// Load the LandTrendr.js module
var ltgee = require('users/emaprlab/public:Modules/LandTrendr.js'); 

// Add index to changeParams object
changeParams_greatest.index = index;

// Run landtrendr
var lt = ltgee.runLT(startYear, endYear, startDay, endDay, aoi, index, [index], runParams, maskThese);

// Get the change map layers
var greatest_change_image = ltgee.getChangeMap(lt, changeParams_greatest);

// Export change data to my google drive
var region = table;

var export_greatest_change_image = greatest_change_image.clip(region).unmask(0).short();


Export.image.toDrive({
  image: export_greatest_change_image, 
  description: 'landtrendr_greatest_change_image_' + country + '_' + index, 
  folder: 'landtrendr/' + country, 
  fileNamePrefix: 'landtrendr_greatest_change_image_' + country + '_' + index, 
  region: region.geometry().bounds(),
  scale: 30,
  crs: 'EPSG:3035', 
  maxPixels: 1e13
});



