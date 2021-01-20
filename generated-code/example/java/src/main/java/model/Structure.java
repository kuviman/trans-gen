package model;

import util.StreamUtil;

public class Structure {
    private model.OneOf oneOfOne;

    public model.OneOf getOneOfOne() {
        return oneOfOne;
    }

    public void setOneOfOne(model.OneOf value) {
        this.oneOfOne = value;
    }
    private model.OneOf oneOfTwo;

    public model.OneOf getOneOfTwo() {
        return oneOfTwo;
    }

    public void setOneOfTwo(model.OneOf value) {
        this.oneOfTwo = value;
    }
    private java.util.Map<model.Enumeration, Integer> hashMap;

    public java.util.Map<model.Enumeration, Integer> getHashMap() {
        return hashMap;
    }

    public void setHashMap(java.util.Map<model.Enumeration, Integer> value) {
        this.hashMap = value;
    }
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

    public Structure(model.OneOf oneOfOne, model.OneOf oneOfTwo, java.util.Map<model.Enumeration, Integer> hashMap, String text, float floatNumber, double doubleNumber) {
        this.oneOfOne = oneOfOne;
        this.oneOfTwo = oneOfTwo;
        this.hashMap = hashMap;
        this.text = text;
        this.floatNumber = floatNumber;
        this.doubleNumber = doubleNumber;
    }

    public static Structure readFrom(java.io.InputStream stream) throws java.io.IOException {
        model.OneOf oneOfOne;
        oneOfOne = model.OneOf.readFrom(stream);
        model.OneOf oneOfTwo;
        oneOfTwo = model.OneOf.readFrom(stream);
        java.util.Map<model.Enumeration, Integer> hashMap;
        int hashMapSize = StreamUtil.readInt(stream);
        hashMap = new java.util.HashMap<>(hashMapSize);
        for (int hashMapIndex = 0; hashMapIndex < hashMapSize; hashMapIndex++) {
            model.Enumeration hashMapKey;
            switch (StreamUtil.readInt(stream)) {
            case 0:
                hashMapKey = model.Enumeration.VALUE_ONE;
                break;
            case 1:
                hashMapKey = model.Enumeration.VALUE_TWO;
                break;
            default:
                throw new java.io.IOException("Unexpected tag value");
            }
            int hashMapValue;
            hashMapValue = StreamUtil.readInt(stream);
            hashMap.put(hashMapKey, hashMapValue);
        }
        String text;
        text = StreamUtil.readString(stream);
        float floatNumber;
        floatNumber = StreamUtil.readFloat(stream);
        double doubleNumber;
        doubleNumber = StreamUtil.readDouble(stream);
        return new Structure(oneOfOne, oneOfTwo, hashMap, text, floatNumber, doubleNumber);
    }

    public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
        oneOfOne.writeTo(stream);
        oneOfTwo.writeTo(stream);
        StreamUtil.writeInt(stream, hashMap.size());
        for (java.util.Map.Entry<model.Enumeration, Integer> hashMapEntry : hashMap.entrySet()) {
            model.Enumeration hashMapKey = hashMapEntry.getKey();
            StreamUtil.writeInt(stream, hashMapKey.tag);
            int hashMapValue = hashMapEntry.getValue();
            StreamUtil.writeInt(stream, hashMapValue);
        }
        StreamUtil.writeString(stream, text);
        StreamUtil.writeFloat(stream, floatNumber);
        StreamUtil.writeDouble(stream, doubleNumber);
    }
}