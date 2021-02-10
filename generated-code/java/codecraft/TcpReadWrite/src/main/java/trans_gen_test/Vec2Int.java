package trans_gen_test;

import trans_gen_test.util.StreamUtil;

/**
 * 2 dimensional vector.
 */
public class Vec2Int {
    /**
     * `x` coordinate of the vector
     */
    private int x;

    /**
     * `x` coordinate of the vector
     */
    public int getX() {
        return x;
    }

    /**
     * `x` coordinate of the vector
     */
    public void setX(int value) {
        this.x = value;
    }
    /**
     * `y` coordinate of the vector
     */
    private int y;

    /**
     * `y` coordinate of the vector
     */
    public int getY() {
        return y;
    }

    /**
     * `y` coordinate of the vector
     */
    public void setY(int value) {
        this.y = value;
    }

    public Vec2Int(int x, int y) {
        this.x = x;
        this.y = y;
    }

    /**
     * Read Vec2Int from input stream
     */
    public static Vec2Int readFrom(java.io.InputStream stream) throws java.io.IOException {
        int x;
        x = StreamUtil.readInt(stream);
        int y;
        y = StreamUtil.readInt(stream);
        return new Vec2Int(x, y);
    }

    /**
     * Write Vec2Int to output stream
     */
    public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
        StreamUtil.writeInt(stream, x);
        StreamUtil.writeInt(stream, y);
    }

    /**
     * Get string representation of Vec2Int
     */
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