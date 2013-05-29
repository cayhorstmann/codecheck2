//REQUIRED [(]\s*Country\s*[)]
// Did you use a cast?

public class LargestTester
{
    public static void main(String[] args)
    {
        Country[] centralAmericanCountries = 
            {
                new Country("Belize", 22966), 
                new Country("Costa Rica", 51100), 
                new Country("El Salvador", 21041), 
                new Country("Guatemala", 108889), 
                new Country("Honduras", 112492), 
                new Country("Nicaragua", 130373), 
                new Country("Panama", 75417)
            };

        Measurable largest = Data.largest(centralAmericanCountries);
        Country largestCountry = (Country) largest;
        System.out.println("Largest country: " + largestCountry.getName());
        System.out.println("Expected: Nicaragua");
    }
}
