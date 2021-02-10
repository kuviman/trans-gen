#include "AttackAction.hpp"

namespace model {

AttackAction::AttackAction(std::optional<int> target, std::optional<model::AutoAttack> autoAttack) : target(target), autoAttack(autoAttack) { }

// Read AttackAction from input stream
AttackAction AttackAction::readFrom(InputStream& stream) {
    std::optional<int> target = std::optional<int>();
    if (stream.readBool()) {
        target = stream.readInt();
    }
    std::optional<model::AutoAttack> autoAttack = std::optional<model::AutoAttack>();
    if (stream.readBool()) {
        autoAttack = model::AutoAttack::readFrom(stream);
    }
    return AttackAction(target, autoAttack);
}

// Write AttackAction to output stream
void AttackAction::writeTo(OutputStream& stream) const {
    if (target) {
        stream.write(true);
        const int& targetValue = *target;
        stream.write(targetValue);
    } else {
        stream.write(false);
    }
    if (autoAttack) {
        stream.write(true);
        const model::AutoAttack& autoAttackValue = *autoAttack;
        autoAttackValue.writeTo(stream);
    } else {
        stream.write(false);
    }
}

// Get string representation of AttackAction
std::string AttackAction::toString() const {
    std::stringstream ss;
    ss << "AttackAction { ";
    ss << "target: ";
    if (target) {
        const int& targetValue = *target;
        ss << targetValue;
    } else {
        ss << "none";
    }
    ss << ", ";
    ss << "autoAttack: ";
    if (autoAttack) {
        const model::AutoAttack& autoAttackValue = *autoAttack;
        ss << autoAttackValue.toString();
    } else {
        ss << "none";
    }
    ss << " }";
    return ss.str();
}

}