package model;

import util.StreamUtil;

public class Player {
    private int id;

    public int getId() {
        return id;
    }

    public void setId(int value) {
        this.id = value;
    }
    private int score;

    public int getScore() {
        return score;
    }

    public void setScore(int value) {
        this.score = value;
    }
    private int resource;

    public int getResource() {
        return resource;
    }

    public void setResource(int value) {
        this.resource = value;
    }

    public Player(int id, int score, int resource) {
        this.id = id;
        this.score = score;
        this.resource = resource;
    }

    public static Player readFrom(java.io.InputStream stream) throws java.io.IOException {
        int id;
        id = StreamUtil.readInt(stream);
        int score;
        score = StreamUtil.readInt(stream);
        int resource;
        resource = StreamUtil.readInt(stream);
        return new Player(id, score, resource);
    }

    public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
        StreamUtil.writeInt(stream, id);
        StreamUtil.writeInt(stream, score);
        StreamUtil.writeInt(stream, resource);
    }

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