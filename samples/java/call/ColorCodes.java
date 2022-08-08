//SOLUTION 
public class ColorCodes 
{ 
    /** Returns the code of the given image when doubled in size 
     * @param img the codes of an image 
     * @return the color codes of the doubled image 
     * */ 
    
//CALL "RGB\nBGR\nGGG\n" 
//CALL "RRR\nRGR\nRRR\n" 
//CALL "R\n" 
//CALL "BB\n" 
public String doubleImage(String img) 
{ 
    //SHOW 
    String result = ""; 
    String row = ""; 
    int lastIndex=0; 
    for (int i = 0; i < img.length(); i++) 
    { 
        char ch = img.charAt(i); 
        if (ch == '\n') 
        { 
            //HIDE 
            row += "\n"; 
            result += "\n"; 
            result += row; 
            row = "";
            //SHOW // your work here 
        } 
        else 
        {
            //HIDE
            row += img.charAt(i); 
            row += img.charAt(i);
            result += img.charAt(i); 
            result += img.charAt(i);
            //SHOW // your work here
        } 
    }
    return result; 
}
}
    