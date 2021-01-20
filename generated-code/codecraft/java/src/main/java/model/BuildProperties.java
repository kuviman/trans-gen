package model;

import util.StreamUtil;

public class BuildProperties {
    private model.EntityType[] options;

    public model.EntityType[] getOptions() {
        return options;
    }

    public void setOptions(model.EntityType[] value) {
        this.options = value;
    }
    private Integer initHealth;

    public Integer getInitHealth() {
        return initHealth;
    }

    public void setInitHealth(Integer value) {
        this.initHealth = value;
    }

    public BuildProperties(model.EntityType[] options, Integer initHealth) {
        this.options = options;
        this.initHealth = initHealth;
    }

    public static BuildProperties readFrom(java.io.InputStream stream) throws java.io.IOException {
        model.EntityType[] options;
        options = new model.EntityType[StreamUtil.readInt(stream)];
        for (int optionsIndex = 0; optionsIndex < options.length; optionsIndex++) {
            model.EntityType optionsElement;
            switch (StreamUtil.readInt(stream)) {
            case 0:
                optionsElement = model.EntityType.WALL;
                break;
            case 1:
                optionsElement = model.EntityType.HOUSE;
                break;
            case 2:
                optionsElement = model.EntityType.BUILDER_BASE;
                break;
            case 3:
                optionsElement = model.EntityType.BUILDER_UNIT;
                break;
            case 4:
                optionsElement = model.EntityType.MELEE_BASE;
                break;
            case 5:
                optionsElement = model.EntityType.MELEE_UNIT;
                break;
            case 6:
                optionsElement = model.EntityType.RANGED_BASE;
                break;
            case 7:
                optionsElement = model.EntityType.RANGED_UNIT;
                break;
            case 8:
                optionsElement = model.EntityType.RESOURCE;
                break;
            case 9:
                optionsElement = model.EntityType.TURRET;
                break;
            default:
                throw new java.io.IOException("Unexpected tag value");
            }
            options[optionsIndex] = optionsElement;
        }
        Integer initHealth;
        if (StreamUtil.readBoolean(stream)) {
            initHealth = StreamUtil.readInt(stream);
        } else {
            initHealth = null;
        }
        return new BuildProperties(options, initHealth);
    }

    public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
        StreamUtil.writeInt(stream, options.length);
        for (model.EntityType optionsElement : options) {
            StreamUtil.writeInt(stream, optionsElement.tag);
        }
        if (initHealth == null) {
            StreamUtil.writeBoolean(stream, false);
        } else {
            StreamUtil.writeBoolean(stream, true);
            StreamUtil.writeInt(stream, initHealth);
        }
    }
}