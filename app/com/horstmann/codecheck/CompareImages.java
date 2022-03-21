package com.horstmann.codecheck;

import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.IOException;
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

    public CompareImages(byte[] firstImage) {
        try {
            image1 = readImage(firstImage);
        } catch (IOException ex) {
            image1 = null;
        }
    }

    public void setOtherImage(byte[] p) throws IOException {
        try {
            image2 = readImage(p);
        } catch (IOException ex) {
            image2 = null;
        }
    }

    public BufferedImage first() { return image1; }
    public BufferedImage other() { return image2; }
    
    private static BufferedImage readImage(byte[] bytes) throws IOException {
        if (bytes == null) throw new IOException("null data");
        return ImageIO.read(new ByteArrayInputStream(bytes));
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

                int cdiff = dr * dr + dg * dg + db * db;
                int rgb;
                if (cdiff == 0) rgb = 0xFFFFFF; 
                else {
                    int THRESHOLD = 200;
                    int MAXCDIFF = 3 * 255 * 255;
                    int gray = THRESHOLD - THRESHOLD * cdiff / MAXCDIFF;
                    rgb = 0xFF0000 + (gray << 8) + gray;
                }
                diff.setRGB(x, y, rgb);
            }
        return diff;
    }
}
