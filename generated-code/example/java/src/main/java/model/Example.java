package model;

import util.StreamUtil;

public class Example {
    private model.OneOf oneOf;

    public model.OneOf getOneOf() {
        return oneOf;
    }

    public void setOneOf(model.OneOf value) {
        this.oneOf = value;
    }
    private java.util.Map<model.Enumeration, Integer> hashMap;

    public java.util.Map<model.Enumeration, Integer> getHashMap() {
        return hashMap;
    }

    public void setHashMap(java.util.Map<model.Enumeration, Integer> value) {
        this.hashMap = value;
    }
    private Integer optionalInt;

    public Integer getOptionalInt() {
        return optionalInt;
    }

    public void setOptionalInt(Integer value) {
        this.optionalInt = value;
    }
    private Boolean optionalBoolean;

    public Boolean getOptionalBoolean() {
        return optionalBoolean;
    }

    public void setOptionalBoolean(Boolean value) {
        this.optionalBoolean = value;
    }
    private model.OneOf optionalOneOf;

    public model.OneOf getOptionalOneOf() {
        return optionalOneOf;
    }

    public void setOptionalOneOf(model.OneOf value) {
        this.optionalOneOf = value;
    }
    private model.Structure optionalStruct;

    public model.Structure getOptionalStruct() {
        return optionalStruct;
    }

    public void setOptionalStruct(model.Structure value) {
        this.optionalStruct = value;
    }
    private model.Enumeration optionalEnum;

    public model.Enumeration getOptionalEnum() {
        return optionalEnum;
    }

    public void setOptionalEnum(model.Enumeration value) {
        this.optionalEnum = value;
    }

    public Example(model.OneOf oneOf, java.util.Map<model.Enumeration, Integer> hashMap, Integer optionalInt, Boolean optionalBoolean, model.OneOf optionalOneOf, model.Structure optionalStruct, model.Enumeration optionalEnum) {
        this.oneOf = oneOf;
        this.hashMap = hashMap;
        this.optionalInt = optionalInt;
        this.optionalBoolean = optionalBoolean;
        this.optionalOneOf = optionalOneOf;
        this.optionalStruct = optionalStruct;
        this.optionalEnum = optionalEnum;
    }

    public static Example readFrom(java.io.InputStream stream) throws java.io.IOException {
        model.OneOf oneOf;
        oneOf = model.OneOf.readFrom(stream);
        java.util.Map<model.Enumeration, Integer> hashMap;
        int hashMapSize = StreamUtil.readInt(stream);
        hashMap = new java.util.HashMap<>(hashMapSize);
        for (int hashMapIndex = 0; hashMapIndex < hashMapSize; hashMapIndex++) {
            model.Enumeration hashMapKey;
            hashMapKey = model.Enumeration.readFrom(stream);
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
        model.OneOf optionalOneOf;
        if (StreamUtil.readBoolean(stream)) {
            optionalOneOf = model.OneOf.readFrom(stream);
        } else {
            optionalOneOf = null;
        }
        model.Structure optionalStruct;
        if (StreamUtil.readBoolean(stream)) {
            optionalStruct = model.Structure.readFrom(stream);
        } else {
            optionalStruct = null;
        }
        model.Enumeration optionalEnum;
        if (StreamUtil.readBoolean(stream)) {
            optionalEnum = model.Enumeration.readFrom(stream);
        } else {
            optionalEnum = null;
        }
        return new Example(oneOf, hashMap, optionalInt, optionalBoolean, optionalOneOf, optionalStruct, optionalEnum);
    }

    public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
        oneOf.writeTo(stream);
        StreamUtil.writeInt(stream, hashMap.size());
        for (java.util.Map.Entry<model.Enumeration, Integer> hashMapEntry : hashMap.entrySet()) {
            model.Enumeration hashMapKey = hashMapEntry.getKey();
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
}