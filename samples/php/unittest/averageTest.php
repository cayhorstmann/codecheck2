<?php
 
use PHPUnit\Framework\TestCase;
 
require_once './average.php';
 
final class averageTest extends TestCase {
  public function test_average() {
    $this->assertEquals(3.5, average(3, 4));
    $this->assertEquals(3, average(3, 3));
    $this->assertEquals(-0.5, average(0, -1));
    // To test failure
    // $this->assertEquals(-0.5, average(0, -2));
  }
}
 
?>
