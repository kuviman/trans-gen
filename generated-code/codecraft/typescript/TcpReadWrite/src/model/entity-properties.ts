import { AttackProperties } from "./attack-properties";
import { BuildProperties } from "./build-properties";
import { RepairProperties } from "./repair-properties";
import { Stream } from "../stream";

/**
 * Entity properties
 */
export class EntityProperties {
    /**
     * Size. Entity has a form of a square with side of this length
     */
    size: number
    /**
     * Score for building this entity
     */
    buildScore: number
    /**
     * Score for destroying this entity
     */
    destroyScore: number
    /**
     * Whether this entity can move
     */
    canMove: boolean
    /**
     * Number of population points this entity provides, if active
     */
    populationProvide: number
    /**
     * Number of population points this entity uses
     */
    populationUse: number
    /**
     * Maximum health points
     */
    maxHealth: number
    /**
     * Cost to build this first entity of this type. If this is a unit (entity can move), the cost is increased by 1 for each existing unit of this type
     */
    initialCost: number
    /**
     * If fog of war is enabled, maximum distance at which other entities are considered visible
     */
    sightRange: number
    /**
     * Amount of resource added to enemy able to collect resource on dealing damage for 1 health point
     */
    resourcePerHealth: number
    /**
     * Build properties, if entity can build
     */
    build: BuildProperties | null
    /**
     * Attack properties, if entity can attack
     */
    attack: AttackProperties | null
    /**
     * Repair properties, if entity can repair
     */
    repair: RepairProperties | null

    constructor(size: number, buildScore: number, destroyScore: number, canMove: boolean, populationProvide: number, populationUse: number, maxHealth: number, initialCost: number, sightRange: number, resourcePerHealth: number, build: BuildProperties | null, attack: AttackProperties | null, repair: RepairProperties | null) {
        this.size = size;
        this.buildScore = buildScore;
        this.destroyScore = destroyScore;
        this.canMove = canMove;
        this.populationProvide = populationProvide;
        this.populationUse = populationUse;
        this.maxHealth = maxHealth;
        this.initialCost = initialCost;
        this.sightRange = sightRange;
        this.resourcePerHealth = resourcePerHealth;
        this.build = build;
        this.attack = attack;
        this.repair = repair;
    }

    /**
     * Read EntityProperties from input stream
     */
    static async readFrom(stream: Stream): Promise<EntityProperties> {
        let size;
        size = await stream.readInt();
        let buildScore;
        buildScore = await stream.readInt();
        let destroyScore;
        destroyScore = await stream.readInt();
        let canMove;
        canMove = await stream.readBool();
        let populationProvide;
        populationProvide = await stream.readInt();
        let populationUse;
        populationUse = await stream.readInt();
        let maxHealth;
        maxHealth = await stream.readInt();
        let initialCost;
        initialCost = await stream.readInt();
        let sightRange;
        sightRange = await stream.readInt();
        let resourcePerHealth;
        resourcePerHealth = await stream.readInt();
        let build;
        if (await stream.readBool()) {
            build = await BuildProperties.readFrom(stream);
        } else {
            build = null;
        }
        let attack;
        if (await stream.readBool()) {
            attack = await AttackProperties.readFrom(stream);
        } else {
            attack = null;
        }
        let repair;
        if (await stream.readBool()) {
            repair = await RepairProperties.readFrom(stream);
        } else {
            repair = null;
        }
        return new EntityProperties(size, buildScore, destroyScore, canMove, populationProvide, populationUse, maxHealth, initialCost, sightRange, resourcePerHealth, build, attack, repair)
    }

    /**
     * Write EntityProperties to output stream
     */
    async writeTo(stream: Stream) {
        let size = this.size;
        await stream.writeInt(size);
        let buildScore = this.buildScore;
        await stream.writeInt(buildScore);
        let destroyScore = this.destroyScore;
        await stream.writeInt(destroyScore);
        let canMove = this.canMove;
        await stream.writeBool(canMove);
        let populationProvide = this.populationProvide;
        await stream.writeInt(populationProvide);
        let populationUse = this.populationUse;
        await stream.writeInt(populationUse);
        let maxHealth = this.maxHealth;
        await stream.writeInt(maxHealth);
        let initialCost = this.initialCost;
        await stream.writeInt(initialCost);
        let sightRange = this.sightRange;
        await stream.writeInt(sightRange);
        let resourcePerHealth = this.resourcePerHealth;
        await stream.writeInt(resourcePerHealth);
        let build = this.build;
        if (build === null) {
            await stream.writeBool(false);
        } else {
            await stream.writeBool(true);
            await build.writeTo(stream);
        }
        let attack = this.attack;
        if (attack === null) {
            await stream.writeBool(false);
        } else {
            await stream.writeBool(true);
            await attack.writeTo(stream);
        }
        let repair = this.repair;
        if (repair === null) {
            await stream.writeBool(false);
        } else {
            await stream.writeBool(true);
            await repair.writeTo(stream);
        }
    }
}