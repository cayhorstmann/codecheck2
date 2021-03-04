/**
   A country with a name and area.
*/
public class Country implements Measurable
{
   private String name;
   private double area;

   /**
      Constructs a country.
      @param aName the name of the country
      @param anArea the area of the country
   */
   public Country(String aName, double anArea) 
   { 
      name = aName;
      area = anArea; 
   }

   /**
      Gets the country name.
      @return the name
   */
   public String getName() 
   {
      return name;
   }

   /**
      Gets the area of the country.
      @return the area
   */
   public double getArea() 
   {
      return area;
   }

   /**
      Gets the area of the country.
      @return the area
   */
   public double getMeasure() 
   {
      return area;
   }
}
