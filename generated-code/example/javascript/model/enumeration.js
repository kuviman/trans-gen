module.exports = {
    ValueOne: 0,
    ValueTwo: 1,
    readFrom: async function(stream) {
        const result = stream.readInt();
        if (result < 0 || result >= 2) {
            throw new Error("Unexpected tag value");
        }
        return result;
    }
};