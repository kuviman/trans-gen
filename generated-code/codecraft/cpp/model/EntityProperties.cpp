#include "EntityProperties.hpp"

EntityProperties::EntityProperties() { }

EntityProperties::EntityProperties(int size, int buildScore, int destroyScore, bool canMove, int populationProvide, int populationUse, int maxHealth, int initialCost, int sightRange, int resourcePerHealth, std::optional<BuildProperties> build, std::optional<AttackProperties> attack, std::optional<RepairProperties> repair) : size(size), buildScore(buildScore), destroyScore(destroyScore), canMove(canMove), populationProvide(populationProvide), populationUse(populationUse), maxHealth(maxHealth), initialCost(initialCost), sightRange(sightRange), resourcePerHealth(resourcePerHealth), build(build), attack(attack), repair(repair) { }

// Read EntityProperties from input stream
EntityProperties EntityProperties::readFrom(InputStream& stream) {
    int size;
    size = stream.readInt();
    int buildScore;
    buildScore = stream.readInt();
    int destroyScore;
    destroyScore = stream.readInt();
    bool canMove;
    canMove = stream.readBool();
    int populationProvide;
    populationProvide = stream.readInt();
    int populationUse;
    populationUse = stream.readInt();
    int maxHealth;
    maxHealth = stream.readInt();
    int initialCost;
    initialCost = stream.readInt();
    int sightRange;
    sightRange = stream.readInt();
    int resourcePerHealth;
    resourcePerHealth = stream.readInt();
    std::optional<BuildProperties> build;
    if (stream.readBool()) {
        BuildProperties buildValue;
        buildValue = BuildProperties::readFrom(stream);
        build = buildValue;
    } else {
        build = std::optional<BuildProperties>();
    }
    std::optional<AttackProperties> attack;
    if (stream.readBool()) {
        AttackProperties attackValue;
        attackValue = AttackProperties::readFrom(stream);
        attack = attackValue;
    } else {
        attack = std::optional<AttackProperties>();
    }
    std::optional<RepairProperties> repair;
    if (stream.readBool()) {
        RepairProperties repairValue;
        repairValue = RepairProperties::readFrom(stream);
        repair = repairValue;
    } else {
        repair = std::optional<RepairProperties>();
    }
    return EntityProperties(size, buildScore, destroyScore, canMove, populationProvide, populationUse, maxHealth, initialCost, sightRange, resourcePerHealth, build, attack, repair);
}

// Write EntityProperties to output stream
void EntityProperties::writeTo(OutputStream& stream) const {
    stream.write(size);
    stream.write(buildScore);
    stream.write(destroyScore);
    stream.write(canMove);
    stream.write(populationProvide);
    stream.write(populationUse);
    stream.write(maxHealth);
    stream.write(initialCost);
    stream.write(sightRange);
    stream.write(resourcePerHealth);
    if (build) {
        stream.write(true);
        const BuildProperties& buildValue = *build;
        buildValue.writeTo(stream);
    } else {
        stream.write(false);
    }
    if (attack) {
        stream.write(true);
        const AttackProperties& attackValue = *attack;
        attackValue.writeTo(stream);
    } else {
        stream.write(false);
    }
    if (repair) {
        stream.write(true);
        const RepairProperties& repairValue = *repair;
        repairValue.writeTo(stream);
    } else {
        stream.write(false);
    }
}

// Get string representation of EntityProperties
std::string EntityProperties::toString() const {
    std::stringstream ss;
    ss << "EntityProperties { ";
    ss << "size: ";
    ss << size;
    ss << ", ";
    ss << "buildScore: ";
    ss << buildScore;
    ss << ", ";
    ss << "destroyScore: ";
    ss << destroyScore;
    ss << ", ";
    ss << "canMove: ";
    ss << canMove;
    ss << ", ";
    ss << "populationProvide: ";
    ss << populationProvide;
    ss << ", ";
    ss << "populationUse: ";
    ss << populationUse;
    ss << ", ";
    ss << "maxHealth: ";
    ss << maxHealth;
    ss << ", ";
    ss << "initialCost: ";
    ss << initialCost;
    ss << ", ";
    ss << "sightRange: ";
    ss << sightRange;
    ss << ", ";
    ss << "resourcePerHealth: ";
    ss << resourcePerHealth;
    ss << ", ";
    ss << "build: ";
    if (build) {
        const BuildProperties& buildValue = *build;
        ss << buildValue.toString();
    } else {
        ss << "none";
    }
    ss << ", ";
    ss << "attack: ";
    if (attack) {
        const AttackProperties& attackValue = *attack;
        ss << attackValue.toString();
    } else {
        ss << "none";
    }
    ss << ", ";
    ss << "repair: ";
    if (repair) {
        const RepairProperties& repairValue = *repair;
        ss << repairValue.toString();
    } else {
        ss << "none";
    }
    ss << " }";
    return ss.str();
}