package model;

import util.StreamUtil;

public class Vec2Int {
    private int x;

    public int getX() {
        return x;
    }

    public void setX(int value) {
        this.x = value;
    }
    private int y;

    public int getY() {
        return y;
    }

    public void setY(int value) {
        this.y = value;
    }

    public Vec2Int(int x, int y) {
        this.x = x;
        this.y = y;
    }

    public static Vec2Int readFrom(java.io.InputStream stream) throws java.io.IOException {
        int x;
        x = StreamUtil.readInt(stream);
        int y;
        y = StreamUtil.readInt(stream);
        return new Vec2Int(x, y);
    }

    public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
        StreamUtil.writeInt(stream, x);
        StreamUtil.writeInt(stream, y);
    }

    @Override
    public String toString() {
        StringBuilder stringBuilder = new StringBuilder("Vec2Int { ");
        stringBuilder.append("x: ");
        stringBuilder.append(String.valueOf(x));
        stringBuilder.append(", ");
        stringBuilder.append("y: ");
        stringBuilder.append(String.valueOf(y));
        stringBuilder.append(" }");
        return stringBuilder.toString();
    }
}