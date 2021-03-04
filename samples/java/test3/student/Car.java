/**
   A simulated car.
*/
public class Car implements Measurable
{
    private String name;
    private double milesPerGallon;

    /**
       Constructs a car with a given fuel efficiency.
       @param modelName the model name
       @param mpg the miles per gallon of this car
    */
    public Car(String modelName, double mpg)
    {
        name = modelName;
        milesPerGallon = mpg;
    }

    /**
       Gets the model name of this car.
       @return the model name
    */
    public String toString()
    {
        return name;
    }
 
    /**
       Gets the fuel efficiency of this car.
       @return the fuel efficiency in miles per gallon
    */
    public double getMeasure()
    {
        return milesPerGallon;
    }
   
    // Other methods omitted for simplicity
}
