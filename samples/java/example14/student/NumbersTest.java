//HIDDEN
import org.junit.Test;
import org.junit.Assert;

public class NumbersTest
{
   @Test public void testNegative()
   {
      Numbers numbers = new Numbers();
      Assert.assertEquals(9, numbers.square(-3));
   }

   @Test public void testPositive()
   {
      Numbers numbers = new Numbers();
      Assert.assertEquals(9, numbers.square(3));
   }

   @Test public void testZero()
   {
      Numbers numbers = new Numbers();
      Assert.assertEquals(0, numbers.square(0));
   }
}
