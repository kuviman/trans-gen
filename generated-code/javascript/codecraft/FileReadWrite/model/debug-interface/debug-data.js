const ColoredVertex = require.main.require('./model/debug-interface/colored-vertex');
const PrimitiveType = require.main.require('./model/debug-interface/primitive-type');

/**
 * Debug data can be drawn in the app
 */
class DebugData {
    /**
     * Read DebugData from input stream
     */
    static async readFrom(stream) {
        let tag = await stream.readInt();
        if (tag == Log.TAG) {
            return await Log.readFrom(stream);
        }
        if (tag == Primitives.TAG) {
            return await Primitives.readFrom(stream);
        }
        if (tag == PlacedText.TAG) {
            return await PlacedText.readFrom(stream);
        }
        throw new Error("Unexpected tag value");
    }
}
/**
 * Log some text
 */
class Log extends DebugData {
    /**
     * Text to show
     */
    text;

    constructor(text) {
        super();
        this.text = text;
    }

    /**
     * Read Log from input stream
     */
    static async readFrom(stream) {
        let text;
        text = await stream.readString();
        return new Log(text);
    }

    /**
     * Write Log to output stream
     */
    async writeTo(stream) {
        await stream.writeInt(Log.TAG);
        let text = this.text;
        await stream.writeString(text);
    }
}

Log.TAG = 0;
DebugData.Log = Log;
/**
 * Draw primitives
 */
class Primitives extends DebugData {
    /**
     * Vertices
     */
    vertices;
    /**
     * Primitive type
     */
    primitiveType;

    constructor(vertices, primitiveType) {
        super();
        this.vertices = vertices;
        this.primitiveType = primitiveType;
    }

    /**
     * Read Primitives from input stream
     */
    static async readFrom(stream) {
        let vertices;
        vertices = [];
        for (let verticesCount = await stream.readInt(); verticesCount > 0; verticesCount--) {
            let verticesElement;
            verticesElement = await ColoredVertex.readFrom(stream);
            vertices.push(verticesElement);
        }
        let primitiveType;
        primitiveType = await PrimitiveType.readFrom(stream);
        return new Primitives(vertices, primitiveType);
    }

    /**
     * Write Primitives to output stream
     */
    async writeTo(stream) {
        await stream.writeInt(Primitives.TAG);
        let vertices = this.vertices;
        await stream.writeInt(vertices.length);
        for (let verticesElement of vertices) {
            await verticesElement.writeTo(stream);
        }
        let primitiveType = this.primitiveType;
        await primitiveType.writeTo(stream);
    }
}

Primitives.TAG = 1;
DebugData.Primitives = Primitives;
/**
 * Draw text
 */
class PlacedText extends DebugData {
    /**
     * Vertex to determine text position and color
     */
    vertex;
    /**
     * Text
     */
    text;
    /**
     * Text alignment (0 means left, 0.5 means center, 1 means right)
     */
    alignment;
    /**
     * Font size in pixels
     */
    size;

    constructor(vertex, text, alignment, size) {
        super();
        this.vertex = vertex;
        this.text = text;
        this.alignment = alignment;
        this.size = size;
    }

    /**
     * Read PlacedText from input stream
     */
    static async readFrom(stream) {
        let vertex;
        vertex = await ColoredVertex.readFrom(stream);
        let text;
        text = await stream.readString();
        let alignment;
        alignment = await stream.readFloat();
        let size;
        size = await stream.readFloat();
        return new PlacedText(vertex, text, alignment, size);
    }

    /**
     * Write PlacedText to output stream
     */
    async writeTo(stream) {
        await stream.writeInt(PlacedText.TAG);
        let vertex = this.vertex;
        await vertex.writeTo(stream);
        let text = this.text;
        await stream.writeString(text);
        let alignment = this.alignment;
        await stream.writeFloat(alignment);
        let size = this.size;
        await stream.writeFloat(size);
    }
}

PlacedText.TAG = 2;
DebugData.PlacedText = PlacedText;
module.exports = DebugData;