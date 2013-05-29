public class Data
{
   /**
      Computes the average measure of the given objects.
      @param objects an array of Measurable objects
      @return the average of the measures
   */
   public static double average(Measurable[] objects)
   {
      double sum = 0;
      for (Measurable obj : objects)
      {
         sum = sum + obj.getMeasure();
      }
      if (objects.length > 0) { return sum / objects.length; }
      else { return 0; }
   }

   /**
      Computes the largest of the given objects.
      @param objects an array of Measurable objects
      @return the object with the largest measure
   */
   public static Measurable largest(Measurable[] objects)
   {
      if (objects.length == 0) { return null; }
      Measurable largestSoFar = objects[0];
      for (int i = 1; i < objects.length; i++)
      {
         Measurable current = objects[i];
         if (current.getMeasure() > largestSoFar.getMeasure())
         {
             largestSoFar = current;
         }
      }
      return largestSoFar;
   }
}
