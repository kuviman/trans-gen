package model;

import util.StreamUtil;

public class Structure {
    private String text;

    public String getText() {
        return text;
    }

    public void setText(String value) {
        this.text = value;
    }
    private float floatNumber;

    public float getFloatNumber() {
        return floatNumber;
    }

    public void setFloatNumber(float value) {
        this.floatNumber = value;
    }
    private double doubleNumber;

    public double getDoubleNumber() {
        return doubleNumber;
    }

    public void setDoubleNumber(double value) {
        this.doubleNumber = value;
    }

    public Structure(String text, float floatNumber, double doubleNumber) {
        this.text = text;
        this.floatNumber = floatNumber;
        this.doubleNumber = doubleNumber;
    }

    public static Structure readFrom(java.io.InputStream stream) throws java.io.IOException {
        String text;
        text = StreamUtil.readString(stream);
        float floatNumber;
        floatNumber = StreamUtil.readFloat(stream);
        double doubleNumber;
        doubleNumber = StreamUtil.readDouble(stream);
        return new Structure(text, floatNumber, doubleNumber);
    }

    public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
        StreamUtil.writeString(stream, text);
        StreamUtil.writeFloat(stream, floatNumber);
        StreamUtil.writeDouble(stream, doubleNumber);
    }

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