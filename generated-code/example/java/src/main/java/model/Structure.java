package model;

import util.StreamUtil;

public class Structure {
    private model.OneOf oneOfOne;
    public model.OneOf getOneOfOne() { return oneOfOne; }
    public void setOneOfOne(model.OneOf oneOfOne) { this.oneOfOne = oneOfOne; }
    private model.OneOf oneOfTwo;
    public model.OneOf getOneOfTwo() { return oneOfTwo; }
    public void setOneOfTwo(model.OneOf oneOfTwo) { this.oneOfTwo = oneOfTwo; }
    private java.util.Map<model.Enumeration, Integer> hashMap;
    public java.util.Map<model.Enumeration, Integer> getHashMap() { return hashMap; }
    public void setHashMap(java.util.Map<model.Enumeration, Integer> hashMap) { this.hashMap = hashMap; }
    private String text;
    public String getText() { return text; }
    public void setText(String text) { this.text = text; }
    private float floatNumber;
    public float getFloatNumber() { return floatNumber; }
    public void setFloatNumber(float floatNumber) { this.floatNumber = floatNumber; }
    private double doubleNumber;
    public double getDoubleNumber() { return doubleNumber; }
    public void setDoubleNumber(double doubleNumber) { this.doubleNumber = doubleNumber; }
    public Structure() {}
    public Structure(model.OneOf oneOfOne, model.OneOf oneOfTwo, java.util.Map<model.Enumeration, Integer> hashMap, String text, float floatNumber, double doubleNumber) {
        this.oneOfOne = oneOfOne;
        this.oneOfTwo = oneOfTwo;
        this.hashMap = hashMap;
        this.text = text;
        this.floatNumber = floatNumber;
        this.doubleNumber = doubleNumber;
    }
    public static Structure readFrom(java.io.InputStream stream) throws java.io.IOException {
        Structure result = new Structure();
        result.oneOfOne = model.OneOf.readFrom(stream);
        result.oneOfTwo = model.OneOf.readFrom(stream);
        int hashMapSize = StreamUtil.readInt(stream);
        result.hashMap = new java.util.HashMap<>(hashMapSize);
        for (int i = 0; i < hashMapSize; i++) {
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
            result.hashMap.put(hashMapKey, hashMapValue);
        }
        result.text = StreamUtil.readString(stream);
        result.floatNumber = StreamUtil.readFloat(stream);
        result.doubleNumber = StreamUtil.readDouble(stream);
        return result;
    }
    public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
        oneOfOne.writeTo(stream);
        oneOfTwo.writeTo(stream);
        StreamUtil.writeInt(stream, hashMap.size());
        for (java.util.Map.Entry<model.Enumeration, Integer> hashMapEntry : hashMap.entrySet()) {
            model.Enumeration hashMapKey = hashMapEntry.getKey();
            int hashMapValue = hashMapEntry.getValue();
            StreamUtil.writeInt(stream, hashMapKey.tag);
            StreamUtil.writeInt(stream, hashMapValue);
        }
        StreamUtil.writeString(stream, text);
        StreamUtil.writeFloat(stream, floatNumber);
        StreamUtil.writeDouble(stream, doubleNumber);
    }
}
