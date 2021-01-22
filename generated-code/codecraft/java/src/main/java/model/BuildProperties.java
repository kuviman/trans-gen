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
            optionsElement = model.EntityType.readFrom(stream);
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

    @Override
    public String toString() {
        StringBuilder stringBuilder = new StringBuilder("BuildProperties { ");
        stringBuilder.append("options: ");
        stringBuilder.append("[ ");
        for (int optionsIndex = 0; optionsIndex < options.length; optionsIndex++) {
            if (optionsIndex != 0) {
                stringBuilder.append(", ");
            }
            model.EntityType optionsElement = options[optionsIndex];
            stringBuilder.append(String.valueOf(optionsElement));
        }
        stringBuilder.append(" ]");
        stringBuilder.append(", ");
        stringBuilder.append("initHealth: ");
        stringBuilder.append(String.valueOf(initHealth));
        stringBuilder.append(" }");
        return stringBuilder.toString();
    }
}