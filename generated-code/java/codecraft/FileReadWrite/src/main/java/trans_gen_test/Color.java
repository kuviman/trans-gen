package trans_gen_test;

import trans_gen_test.util.StreamUtil;

/**
 * RGBA Color
 */
public class Color {
    /**
     * Red component
     */
    private float r;

    /**
     * Red component
     */
    public float getR() {
        return r;
    }

    /**
     * Red component
     */
    public void setR(float value) {
        this.r = value;
    }
    /**
     * Green component
     */
    private float g;

    /**
     * Green component
     */
    public float getG() {
        return g;
    }

    /**
     * Green component
     */
    public void setG(float value) {
        this.g = value;
    }
    /**
     * Blue component
     */
    private float b;

    /**
     * Blue component
     */
    public float getB() {
        return b;
    }

    /**
     * Blue component
     */
    public void setB(float value) {
        this.b = value;
    }
    /**
     * Alpha (opacity) component
     */
    private float a;

    /**
     * Alpha (opacity) component
     */
    public float getA() {
        return a;
    }

    /**
     * Alpha (opacity) component
     */
    public void setA(float value) {
        this.a = value;
    }

    public Color(float r, float g, float b, float a) {
        this.r = r;
        this.g = g;
        this.b = b;
        this.a = a;
    }

    /**
     * Read Color from input stream
     */
    public static Color readFrom(java.io.InputStream stream) throws java.io.IOException {
        float r;
        r = StreamUtil.readFloat(stream);
        float g;
        g = StreamUtil.readFloat(stream);
        float b;
        b = StreamUtil.readFloat(stream);
        float a;
        a = StreamUtil.readFloat(stream);
        return new Color(r, g, b, a);
    }

    /**
     * Write Color to output stream
     */
    public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
        StreamUtil.writeFloat(stream, r);
        StreamUtil.writeFloat(stream, g);
        StreamUtil.writeFloat(stream, b);
        StreamUtil.writeFloat(stream, a);
    }

    /**
     * Get string representation of Color
     */
    @Override
    public String toString() {
        StringBuilder stringBuilder = new StringBuilder("Color { ");
        stringBuilder.append("r: ");
        stringBuilder.append(String.valueOf(r));
        stringBuilder.append(", ");
        stringBuilder.append("g: ");
        stringBuilder.append(String.valueOf(g));
        stringBuilder.append(", ");
        stringBuilder.append("b: ");
        stringBuilder.append(String.valueOf(b));
        stringBuilder.append(", ");
        stringBuilder.append("a: ");
        stringBuilder.append(String.valueOf(a));
        stringBuilder.append(" }");
        return stringBuilder.toString();
    }
}