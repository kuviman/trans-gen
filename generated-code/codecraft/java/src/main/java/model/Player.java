package model;

import util.StreamUtil;

/**
 * Player (strategy, client)
 */
public class Player {
    /**
     * Player's ID
     */
    private int id;

    /**
     * Player's ID
     */
    public int getId() {
        return id;
    }

    /**
     * Player's ID
     */
    public void setId(int value) {
        this.id = value;
    }
    /**
     * Current score
     */
    private int score;

    /**
     * Current score
     */
    public int getScore() {
        return score;
    }

    /**
     * Current score
     */
    public void setScore(int value) {
        this.score = value;
    }
    /**
     * Current amount of resource
     */
    private int resource;

    /**
     * Current amount of resource
     */
    public int getResource() {
        return resource;
    }

    /**
     * Current amount of resource
     */
    public void setResource(int value) {
        this.resource = value;
    }

    public Player(int id, int score, int resource) {
        this.id = id;
        this.score = score;
        this.resource = resource;
    }

    /**
     * Read Player from input stream
     */
    public static Player readFrom(java.io.InputStream stream) throws java.io.IOException {
        int id;
        id = StreamUtil.readInt(stream);
        int score;
        score = StreamUtil.readInt(stream);
        int resource;
        resource = StreamUtil.readInt(stream);
        return new Player(id, score, resource);
    }

    /**
     * Write Player to output stream
     */
    public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
        StreamUtil.writeInt(stream, id);
        StreamUtil.writeInt(stream, score);
        StreamUtil.writeInt(stream, resource);
    }

    /**
     * Get string representation of Player
     */
    @Override
    public String toString() {
        StringBuilder stringBuilder = new StringBuilder("Player { ");
        stringBuilder.append("id: ");
        stringBuilder.append(String.valueOf(id));
        stringBuilder.append(", ");
        stringBuilder.append("score: ");
        stringBuilder.append(String.valueOf(score));
        stringBuilder.append(", ");
        stringBuilder.append("resource: ");
        stringBuilder.append(String.valueOf(resource));
        stringBuilder.append(" }");
        return stringBuilder.toString();
    }
}