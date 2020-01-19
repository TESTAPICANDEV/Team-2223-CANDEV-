<?php

$filename = 'vessel.csv';
$mmsi = $_REQUEST['mmsi'];
echo $mmsi;
// The nested array to hold all the arrays
$the_big_array = []; 

// Open the file for reading
if (($h = fopen("{$filename}", "r")) !== FALSE) 
{
  // Each line in the file is converted into an individual array that we call $data
  // The items of the array are comma separated
  while (($data = fgetcsv($h, 1000, ",")) !== FALSE) 
  {
    // Each individual array is being pushed into the nested array
    if ($mmsi == $data[15]){
	echo "asdfasd ".$data[15];
    $the_big_array[] = $data;}		
  }

  // Close the file
  fclose($h);
}

// Display the code in a readable format
echo "<pre>";
var_dump($the_big_array);
echo "</pre>";
