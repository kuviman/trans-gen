import { AttackProperties } from "./attack-properties";
import { BuildProperties } from "./build-properties";
import { RepairProperties } from "./repair-properties";
import { StreamWrapper } from "../stream-wrapper";

export class EntityProperties {
    size: number
    buildScore: number
    destroyScore: number
    canMove: boolean
    populationProvide: number
    populationUse: number
    maxHealth: number
    initialCost: number
    sightRange: number
    resourcePerHealth: number
    build: BuildProperties | null
    attack: AttackProperties | null
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

    static async readFrom(stream: StreamWrapper): Promise<EntityProperties> {
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

    async writeTo(stream: StreamWrapper) {
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
