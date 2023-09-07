//This script is meant to be run within the Google Earth Engine Code Editor
//It displays a 3-year composite image over a lat/lon for pre-aid years
// Specify location and aid year
var lat = 13.545;
var lon = 2.139;
var aidYear = 2002; 

// function to select Red Green Blue bands for the specific Landsat collection
function selectRGBBands(image) {
  var collectionType = image.get('SPACECRAFT_ID');
  
  // Default band order for RGB based on Landsat 5 & 7 bands
  var bandOrder = ['B3', 'B2', 'B1']; 
  
  // Adjust band order for Landsat 8
  if (collectionType === 'LANDSAT_8') {
    bandOrder = ['B4', 'B3', 'B2'];
  }
  
  return image.select(bandOrder);
}

// Function to decode the QA mask
function decodeQAMask(img) {
  var qa = img.select('pixel_qa');
  var clear = qa.bitwiseAnd(2).neq(0); // 0 = not clear, 1 = clear
  clear = clear.updateMask(clear).rename(['pxqa_clear']);

  var water = qa.bitwiseAnd(4).neq(0); // 0 = not water, 1 = water
  water = water.updateMask(water).rename(['pxqa_water']);

  var cloudShadow = qa.bitwiseAnd(8).eq(0); // 0 = shadow, 1 = not shadow
  cloudShadow = cloudShadow.updateMask(cloudShadow).rename(['pxqa_cloudshadow']);

  var snow = qa.bitwiseAnd(16).eq(0); // 0 = snow, 1 = not snow
  snow = snow.updateMask(snow).rename(['pxqa_snow']);

  var cloud = qa.bitwiseAnd(32).eq(0); // 0 = cloud, 1 = not cloud
  cloud = cloud.updateMask(cloud).rename(['pxqa_cloud']);

  var masks = ee.Image.cat([clear, water, cloudShadow, snow, cloud]);
  return masks;
}

// Function to mask out unusable pixels
function maskQAClear(img) {
  var qam = decodeQAMask(img);
  var cloudShadowMask = qam.select('pxqa_cloudshadow');
  var snowMask = qam.select('pxqa_snow');
  var cloudMask = qam.select('pxqa_cloud');
  return img.updateMask(cloudShadowMask).updateMask(snowMask).updateMask(cloudMask);
}

// Define the Landsat collections
var l8 = ee.ImageCollection('LANDSAT/LC08/C01/T1_SR');
var l7 = ee.ImageCollection('LANDSAT/LE07/C01/T1_SR');
var l5 = ee.ImageCollection('LANDSAT/LT05/C01/T1_SR');

// Define the time spans
var spanStartYear, spanEndYear;

if (aidYear >= 1999 && aidYear <= 2001) {
  spanStartYear = 1996;
  spanEndYear = 1998;
} else if (aidYear >= 2002 && aidYear <= 2004) {
  spanStartYear = 1999;
  spanEndYear = 2001;
} else if (aidYear >= 2005 && aidYear <= 2007) {
  spanStartYear = 2002;
  spanEndYear = 2004;
} else if (aidYear >= 2008 && aidYear <= 2010) {
  spanStartYear = 2005;
  spanEndYear = 2007;
} else if (aidYear >= 2011 && aidYear <= 2013) {
  spanStartYear = 2008;
  spanEndYear = 2010;
} else if (aidYear >= 2014 && aidYear <= 2016) {
  spanStartYear = 2011;
  spanEndYear = 2013;
}

// Create a date range for the selected time span
var startDate = ee.Date.fromYMD(spanStartYear, 1, 1);
var endDate = ee.Date.fromYMD(spanEndYear, 12, 31);

// Filter the Landsat collections by date, remove clouds and shadows;
var l5Filtered = l5.filterDate(startDate, endDate).map(maskQAClear);
var l7Filtered = l7.filterDate(startDate, endDate).map(maskQAClear);
var l8Filtered = l8.filterDate(startDate, endDate).map(maskQAClear);

// Select RGB bands based on collection type
var rgbL8 = selectRGBBands(l8Filtered);
var rgbL7 = selectRGBBands(l7Filtered);
var rgbL5 = selectRGBBands(l5Filtered)

var mergedRGB = rgbL5.merge(rgbL7).merge(rgbL8).sort('system:time_start').median();

// Create a 6.7km square box around lat/lon point
var point = ee.Geometry.Point(lon, lat);
var box = point.buffer(3800);
var poi_box_bounds = box.bounds();
var boundingBox = ee.Feature(poi_box_bounds, {});
var outline = ee.Image().toByte().paint({
  featureCollection: ee.FeatureCollection([boundingBox]),
  width: 3, 
});

// Center the map on the square area
Map.centerObject(boundingBox, 12);

// Create a map centered at the specified location and add the RGB image
Map.addLayer(mergedRGB, { min: 0, max: 3000 }, 'RGB Image');
Map.addLayer(outline, {palette: 'ffffff', max: 1}, '6.7km2 outline');
