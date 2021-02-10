package trans_gen_test;

import trans_gen_test.util.StreamUtil;

/**
 * 2 dimensional vector.
 */
public class Vec2Float {
    /**
     * `x` coordinate of the vector
     */
    private float x;

    /**
     * `x` coordinate of the vector
     */
    public float getX() {
        return x;
    }

    /**
     * `x` coordinate of the vector
     */
    public void setX(float value) {
        this.x = value;
    }
    /**
     * `y` coordinate of the vector
     */
    private float y;

    /**
     * `y` coordinate of the vector
     */
    public float getY() {
        return y;
    }

    /**
     * `y` coordinate of the vector
     */
    public void setY(float value) {
        this.y = value;
    }

    public Vec2Float(float x, float y) {
        this.x = x;
        this.y = y;
    }

    /**
     * Read Vec2Float from input stream
     */
    public static Vec2Float readFrom(java.io.InputStream stream) throws java.io.IOException {
        float x;
        x = StreamUtil.readFloat(stream);
        float y;
        y = StreamUtil.readFloat(stream);
        return new Vec2Float(x, y);
    }

    /**
     * Write Vec2Float to output stream
     */
    public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
        StreamUtil.writeFloat(stream, x);
        StreamUtil.writeFloat(stream, y);
    }

    /**
     * Get string representation of Vec2Float
     */
    @Override
    public String toString() {
        StringBuilder stringBuilder = new StringBuilder("Vec2Float { ");
        stringBuilder.append("x: ");
        stringBuilder.append(String.valueOf(x));
        stringBuilder.append(", ");
        stringBuilder.append("y: ");
        stringBuilder.append(String.valueOf(y));
        stringBuilder.append(" }");
        return stringBuilder.toString();
    }
}