<?php
//IN 3\n-3\n0\n
 
$done = false;
while ( !$done ) {
  $n = (int) fgets(STDIN);
  if ( $n == 0 ) {
    $done = true;
  } else {
    //HIDE
    $square = $n * $n;
    //EDIT ...    
    echo "The square is $square\n";
  }
}
 
?>
