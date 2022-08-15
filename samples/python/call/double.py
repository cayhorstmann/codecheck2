##SOLUTION
##CALL "RGB\nBGR\nGGG\n" 
##CALL "RRR\nRGR\nRRR\n" 
##CALL "R\n" 
##CALL "RB\nBR\n" 
def doubleImage(img):  
    ##SHOW 
    result = "" 
    row = "" 
    lastIndex = 0; 
    for i in range(0, len(img)): 
        ch = img[i]
        if (ch == '\n'): 
            ##HIDE 
            row += "\n" 
            result += "\n" 
            result += row 
            row = ""
            ##SHOW // your work here  
        else: 
            ##HIDE
            row += img[i] 
            row += img[i]
            result += img[i] 
            result += img[i]
            ##SHOW // your work here
    return result
    