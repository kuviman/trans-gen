package trans_gen_test;

import trans_gen_test.util.StreamUtil;

/**
 * Example
 */
public class Example {
    /**
     * OneOf
     */
    private trans_gen_test.OneOf oneOf;

    /**
     * OneOf
     */
    public trans_gen_test.OneOf getOneOf() {
        return oneOf;
    }

    /**
     * OneOf
     */
    public void setOneOf(trans_gen_test.OneOf value) {
        this.oneOf = value;
    }
    /**
     * Dictionary
     */
    private java.util.Map<trans_gen_test.Enumeration, Integer> hashMap;

    /**
     * Dictionary
     */
    public java.util.Map<trans_gen_test.Enumeration, Integer> getHashMap() {
        return hashMap;
    }

    /**
     * Dictionary
     */
    public void setHashMap(java.util.Map<trans_gen_test.Enumeration, Integer> value) {
        this.hashMap = value;
    }
    /**
     * Optional int
     */
    private Integer optionalInt;

    /**
     * Optional int
     */
    public Integer getOptionalInt() {
        return optionalInt;
    }

    /**
     * Optional int
     */
    public void setOptionalInt(Integer value) {
        this.optionalInt = value;
    }
    /**
     * Optional boolean
     */
    private Boolean optionalBoolean;

    /**
     * Optional boolean
     */
    public Boolean getOptionalBoolean() {
        return optionalBoolean;
    }

    /**
     * Optional boolean
     */
    public void setOptionalBoolean(Boolean value) {
        this.optionalBoolean = value;
    }
    /**
     * Optional OneOf
     */
    private trans_gen_test.OneOf optionalOneOf;

    /**
     * Optional OneOf
     */
    public trans_gen_test.OneOf getOptionalOneOf() {
        return optionalOneOf;
    }

    /**
     * Optional OneOf
     */
    public void setOptionalOneOf(trans_gen_test.OneOf value) {
        this.optionalOneOf = value;
    }
    /**
     * Optional struct
     */
    private trans_gen_test.Structure optionalStruct;

    /**
     * Optional struct
     */
    public trans_gen_test.Structure getOptionalStruct() {
        return optionalStruct;
    }

    /**
     * Optional struct
     */
    public void setOptionalStruct(trans_gen_test.Structure value) {
        this.optionalStruct = value;
    }
    /**
     * Optional enum
     */
    private trans_gen_test.Enumeration optionalEnum;

    /**
     * Optional enum
     */
    public trans_gen_test.Enumeration getOptionalEnum() {
        return optionalEnum;
    }

    /**
     * Optional enum
     */
    public void setOptionalEnum(trans_gen_test.Enumeration value) {
        this.optionalEnum = value;
    }

    public Example(trans_gen_test.OneOf oneOf, java.util.Map<trans_gen_test.Enumeration, Integer> hashMap, Integer optionalInt, Boolean optionalBoolean, trans_gen_test.OneOf optionalOneOf, trans_gen_test.Structure optionalStruct, trans_gen_test.Enumeration optionalEnum) {
        this.oneOf = oneOf;
        this.hashMap = hashMap;
        this.optionalInt = optionalInt;
        this.optionalBoolean = optionalBoolean;
        this.optionalOneOf = optionalOneOf;
        this.optionalStruct = optionalStruct;
        this.optionalEnum = optionalEnum;
    }

    /**
     * Read Example from input stream
     */
    public static Example readFrom(java.io.InputStream stream) throws java.io.IOException {
        trans_gen_test.OneOf oneOf;
        oneOf = trans_gen_test.OneOf.readFrom(stream);
        java.util.Map<trans_gen_test.Enumeration, Integer> hashMap;
        int hashMapSize = StreamUtil.readInt(stream);
        hashMap = new java.util.HashMap<>(hashMapSize);
        for (int hashMapIndex = 0; hashMapIndex < hashMapSize; hashMapIndex++) {
            trans_gen_test.Enumeration hashMapKey;
            hashMapKey = trans_gen_test.Enumeration.readFrom(stream);
            int hashMapValue;
            hashMapValue = StreamUtil.readInt(stream);
            hashMap.put(hashMapKey, hashMapValue);
        }
        Integer optionalInt;
        if (StreamUtil.readBoolean(stream)) {
            optionalInt = StreamUtil.readInt(stream);
        } else {
            optionalInt = null;
        }
        Boolean optionalBoolean;
        if (StreamUtil.readBoolean(stream)) {
            optionalBoolean = StreamUtil.readBoolean(stream);
        } else {
            optionalBoolean = null;
        }
        trans_gen_test.OneOf optionalOneOf;
        if (StreamUtil.readBoolean(stream)) {
            optionalOneOf = trans_gen_test.OneOf.readFrom(stream);
        } else {
            optionalOneOf = null;
        }
        trans_gen_test.Structure optionalStruct;
        if (StreamUtil.readBoolean(stream)) {
            optionalStruct = trans_gen_test.Structure.readFrom(stream);
        } else {
            optionalStruct = null;
        }
        trans_gen_test.Enumeration optionalEnum;
        if (StreamUtil.readBoolean(stream)) {
            optionalEnum = trans_gen_test.Enumeration.readFrom(stream);
        } else {
            optionalEnum = null;
        }
        return new Example(oneOf, hashMap, optionalInt, optionalBoolean, optionalOneOf, optionalStruct, optionalEnum);
    }

    /**
     * Write Example to output stream
     */
    public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
        oneOf.writeTo(stream);
        StreamUtil.writeInt(stream, hashMap.size());
        for (java.util.Map.Entry<trans_gen_test.Enumeration, Integer> hashMapEntry : hashMap.entrySet()) {
            trans_gen_test.Enumeration hashMapKey = hashMapEntry.getKey();
            StreamUtil.writeInt(stream, hashMapKey.tag);
            int hashMapValue = hashMapEntry.getValue();
            StreamUtil.writeInt(stream, hashMapValue);
        }
        if (optionalInt == null) {
            StreamUtil.writeBoolean(stream, false);
        } else {
            StreamUtil.writeBoolean(stream, true);
            StreamUtil.writeInt(stream, optionalInt);
        }
        if (optionalBoolean == null) {
            StreamUtil.writeBoolean(stream, false);
        } else {
            StreamUtil.writeBoolean(stream, true);
            StreamUtil.writeBoolean(stream, optionalBoolean);
        }
        if (optionalOneOf == null) {
            StreamUtil.writeBoolean(stream, false);
        } else {
            StreamUtil.writeBoolean(stream, true);
            optionalOneOf.writeTo(stream);
        }
        if (optionalStruct == null) {
            StreamUtil.writeBoolean(stream, false);
        } else {
            StreamUtil.writeBoolean(stream, true);
            optionalStruct.writeTo(stream);
        }
        if (optionalEnum == null) {
            StreamUtil.writeBoolean(stream, false);
        } else {
            StreamUtil.writeBoolean(stream, true);
            StreamUtil.writeInt(stream, optionalEnum.tag);
        }
    }

    /**
     * Get string representation of Example
     */
    @Override
    public String toString() {
        StringBuilder stringBuilder = new StringBuilder("Example { ");
        stringBuilder.append("oneOf: ");
        stringBuilder.append(String.valueOf(oneOf));
        stringBuilder.append(", ");
        stringBuilder.append("hashMap: ");
        stringBuilder.append(String.valueOf(hashMap));
        stringBuilder.append(", ");
        stringBuilder.append("optionalInt: ");
        stringBuilder.append(String.valueOf(optionalInt));
        stringBuilder.append(", ");
        stringBuilder.append("optionalBoolean: ");
        stringBuilder.append(String.valueOf(optionalBoolean));
        stringBuilder.append(", ");
        stringBuilder.append("optionalOneOf: ");
        stringBuilder.append(String.valueOf(optionalOneOf));
        stringBuilder.append(", ");
        stringBuilder.append("optionalStruct: ");
        stringBuilder.append(String.valueOf(optionalStruct));
        stringBuilder.append(", ");
        stringBuilder.append("optionalEnum: ");
        stringBuilder.append(String.valueOf(optionalEnum));
        stringBuilder.append(" }");
        return stringBuilder.toString();
    }
}