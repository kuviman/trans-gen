import { ColoredVertex } from "./colored-vertex";
import { PrimitiveType } from "./primitive-type";
import { Stream } from "../../stream";

/**
 * Debug data can be drawn in the app
 */
export abstract class DebugData {
    /**
     * Write DebugData to output stream
     */
    abstract writeTo(stream: Stream): Promise<void>;

    /**
     * Read DebugData from input stream
     */
    static async readFrom(stream: Stream): Promise<DebugData> {
        const tag = await stream.readInt();
        if (tag == DebugData.Log.TAG) {
            return await DebugData.Log.readFrom(stream);
        }
        if (tag == DebugData.Primitives.TAG) {
            return await DebugData.Primitives.readFrom(stream);
        }
        if (tag == DebugData.PlacedText.TAG) {
            return await DebugData.PlacedText.readFrom(stream);
        }
        throw new Error("Unexpected tag value");
    }
}

export namespace DebugData {
    /**
     * Log some text
     */
    export class Log extends DebugData {
        /**
         * Text to show
         */
        text: string
    
        constructor(text: string) {
            super();
            this.text = text;
        }
    
        /**
         * Read Log from input stream
         */
        static async readFrom(stream: Stream): Promise<DebugData.Log> {
            let text;
            text = await stream.readString();
            return new Log(text)
        }
    
        /**
         * Write Log to output stream
         */
        async writeTo(stream: Stream) {
            await stream.writeInt(Log.TAG);
            let text = this.text;
            await stream.writeString(text);
        }
    }
    
    export namespace Log {
        export const TAG = 0;
    }
    /**
     * Draw primitives
     */
    export class Primitives extends DebugData {
        /**
         * Vertices
         */
        vertices: Array<ColoredVertex>
        /**
         * Primitive type
         */
        primitiveType: PrimitiveType
    
        constructor(vertices: Array<ColoredVertex>, primitiveType: PrimitiveType) {
            super();
            this.vertices = vertices;
            this.primitiveType = primitiveType;
        }
    
        /**
         * Read Primitives from input stream
         */
        static async readFrom(stream: Stream): Promise<DebugData.Primitives> {
            let vertices;
            vertices = [];
            for (let verticesCount = await stream.readInt(); verticesCount > 0; verticesCount--) {
                let verticesElement;
                verticesElement = await ColoredVertex.readFrom(stream);
                vertices.push(verticesElement);
            }
            let primitiveType;
            primitiveType = await PrimitiveType.readFrom(stream);
            return new Primitives(vertices, primitiveType)
        }
    
        /**
         * Write Primitives to output stream
         */
        async writeTo(stream: Stream) {
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
    
    export namespace Primitives {
        export const TAG = 1;
    }
    /**
     * Draw text
     */
    export class PlacedText extends DebugData {
        /**
         * Vertex to determine text position and color
         */
        vertex: ColoredVertex
        /**
         * Text
         */
        text: string
        /**
         * Text alignment (0 means left, 0.5 means center, 1 means right)
         */
        alignment: number
        /**
         * Font size in pixels
         */
        size: number
    
        constructor(vertex: ColoredVertex, text: string, alignment: number, size: number) {
            super();
            this.vertex = vertex;
            this.text = text;
            this.alignment = alignment;
            this.size = size;
        }
    
        /**
         * Read PlacedText from input stream
         */
        static async readFrom(stream: Stream): Promise<DebugData.PlacedText> {
            let vertex;
            vertex = await ColoredVertex.readFrom(stream);
            let text;
            text = await stream.readString();
            let alignment;
            alignment = await stream.readFloat();
            let size;
            size = await stream.readFloat();
            return new PlacedText(vertex, text, alignment, size)
        }
    
        /**
         * Write PlacedText to output stream
         */
        async writeTo(stream: Stream) {
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
    
    export namespace PlacedText {
        export const TAG = 2;
    }
}