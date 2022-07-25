//HIDDEN
public class NumbersTester
{
   public static void main(String[] args)
   {
      Numbers nums = new Numbers();
      System.out.println(nums.square(3));
      System.out.println("Expected: 9");
      System.out.println(nums.square(-3));
      System.out.println("Expected: 9");
   }
}
