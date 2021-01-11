mod model;

fn main() {
    let snapshot: model::PlayerView =
        serde_json::from_str(include_str!("snapshot.json")).expect("Failed to read snapshot");
}
