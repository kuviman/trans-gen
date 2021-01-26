package model;

import util.StreamUtil;

/**
 * Example structure
 */
public class Structure {
    /**
     * Text
     */
    private String text;

    /**
     * Text
     */
    public String getText() {
        return text;
    }

    /**
     * Text
     */
    public void setText(String value) {
        this.text = value;
    }
    /**
     * 32-bit float
     */
    private float floatNumber;

    /**
     * 32-bit float
     */
    public float getFloatNumber() {
        return floatNumber;
    }

    /**
     * 32-bit float
     */
    public void setFloatNumber(float value) {
        this.floatNumber = value;
    }
    /**
     * 64-bit float
     */
    private double doubleNumber;

    /**
     * 64-bit float
     */
    public double getDoubleNumber() {
        return doubleNumber;
    }

    /**
     * 64-bit float
     */
    public void setDoubleNumber(double value) {
        this.doubleNumber = value;
    }

    public Structure(String text, float floatNumber, double doubleNumber) {
        this.text = text;
        this.floatNumber = floatNumber;
        this.doubleNumber = doubleNumber;
    }

    /**
     * Read Structure from input stream
     */
    public static Structure readFrom(java.io.InputStream stream) throws java.io.IOException {
        String text;
        text = StreamUtil.readString(stream);
        float floatNumber;
        floatNumber = StreamUtil.readFloat(stream);
        double doubleNumber;
        doubleNumber = StreamUtil.readDouble(stream);
        return new Structure(text, floatNumber, doubleNumber);
    }

    /**
     * Write Structure to output stream
     */
    public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
        StreamUtil.writeString(stream, text);
        StreamUtil.writeFloat(stream, floatNumber);
        StreamUtil.writeDouble(stream, doubleNumber);
    }

    /**
     * Get string representation of Structure
     */
    @Override
    public String toString() {
        StringBuilder stringBuilder = new StringBuilder("Structure { ");
        stringBuilder.append("text: ");
        stringBuilder.append('"' + text + '"');
        stringBuilder.append(", ");
        stringBuilder.append("floatNumber: ");
        stringBuilder.append(String.valueOf(floatNumber));
        stringBuilder.append(", ");
        stringBuilder.append("doubleNumber: ");
        stringBuilder.append(String.valueOf(doubleNumber));
        stringBuilder.append(" }");
        return stringBuilder.toString();
    }
}