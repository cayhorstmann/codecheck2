package com.horstmann.codecheck;

import java.awt.image.BufferedImage;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;

import javax.imageio.ImageIO;

public class CompareImages {
    private BufferedImage image1;
    private BufferedImage image2;

    public static boolean isImage(String name) {
        if (name == null) return false;
        String extension = name.substring(name.lastIndexOf(".") + 1).toLowerCase();
        return Arrays.asList(ImageIO.getReaderFileSuffixes()).contains(extension);
    }

    public CompareImages(Path firstImage) {
        image1 = readImage(firstImage);
    }

    public void setOtherImage(Path p) {
        image2 = readImage(p);
    }

    private static BufferedImage readImage(Path p) {
    	int tries = 10;
    	while (tries > 0) {
    		tries--;
    		if (Files.exists(p)) {
    	    	tries = 10;
    	    	while (tries > 0) {
    	    		tries--;
    	    		try {            
    	    			return ImageIO.read(p.toFile());
    	    		} catch (Exception ex) {
    	    		}
    	    		if (tries == 0) throw new RuntimeException(p + " not readable");
    	            try {
    	                Thread.sleep(1000);
    	            } catch (InterruptedException e) {}    		    		
    	    	}    			
    		}
    		else {
                try {
                    Thread.sleep(1000);
                } catch (InterruptedException e) {}
    		}    		
    	}
		throw new RuntimeException(p + " not found");
    }

    public boolean getOutcome() {
        if (image1 == null || image2 == null) return false;
        if (image1.getWidth() != image2.getWidth() || image1.getHeight() != image2.getHeight())
            return false;
        for (int x = 0; x < image1.getWidth(); x++)
            for (int y = 0; y < image1.getHeight(); y++)
                if ((image1.getRGB(x, y) & 0xFFFFFF) != (image2.getRGB(x, y) & 0xFFFFFF))
                    return false;
        return true;
    }

    public BufferedImage diff() {
        if (image1 == null || image2 == null) return null;
        BufferedImage diff = new BufferedImage(Math.max(image1.getWidth(), image2.getWidth()),
                                               Math.max(image1.getHeight(), image2.getHeight()), BufferedImage.TYPE_INT_RGB);

        for (int x = 0; x < diff.getWidth(); x++)
            for (int y = 0; y < diff.getHeight(); y++) {
                int rgb1 = 0xFFFFFF;
                int rgb2 = 0xFFFFFF;
                if (x < image1.getWidth() && y < image1.getHeight())
                	rgb1 = image1.getRGB(x, y) & 0xFFFFFF;
                if (x < image2.getWidth() && y < image2.getHeight())                     
                    rgb2 = image2.getRGB(x, y) & 0xFFFFFF;
                int dr = ((rgb1 >> 16) & 0xFF) - ((rgb2 >> 16) & 0xFF);
                int dg = ((rgb1 >> 8) & 0xFF) - ((rgb2 >> 8) & 0xFF);
                int db = (rgb1 & 0xFF) - (rgb2 & 0xFF);

                int cdiff = Math.min(255, (dr * dr + dg * dg + db * db) /
                                     255);
                int rgb = 0xFF0000 + ((255 - cdiff) << 8) + 255 - cdiff;
                diff.setRGB(x, y, rgb);
            }
        return diff;
    }
}
