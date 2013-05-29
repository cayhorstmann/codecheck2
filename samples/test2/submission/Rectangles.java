public class Rectangles
{
    public static void main(String[] args)
    {
        Rectangle box = new Rectangle(0, 0, 50, 60);
        box.draw();
        // Draw another rectangle of the same size that touches
        // box at the bottom right corner
        Rectangle box2 = new Rectangle(49, 59, 50, 60);
        box2.draw();        
    }
}
