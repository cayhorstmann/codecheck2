<?php
 
//CALL [1, 2, -3, -4, -5, 9, 9999]
//CALL [-1, -2, -3, 9999, 100]
//CALL [9999]
function average_rainfall( $rainfall ) {
  //HIDE
  $sum = 0;
  $i = 0;
  $count = 0;
  while ( $rainfall[$i] != 9999 ) {
    if ( $rainfall[$i] >= 0 ) {
      $sum += $rainfall[$i];
      $count++;
    }
    $i++;
  }
  if ( $count == 0 ) {
    return 0;
  } else {
    return $sum / $count;
  }
  //EDIT ...
}
  
?>
