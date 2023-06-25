use std::env;
use std::fs;
use std::io::Write;
use std::path::Path;

use serde_json::Value;

/// Capitalize values and join them with comma
fn to_enum_body(values: Vec<String>) -> String {
    values
        .iter()
        .map(|v| format!("{}{}", v[0..1].to_uppercase(), &v[1..]))
        .collect::<Vec<_>>()
        .join(", ")
}

fn main() {
    println!("cargo:rerun-if-changed=server.config.json");

    let out_dir = env::var_os("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("tokens.rs");
    println!("{}", dest_path.display());

    let config: Value = serde_json::from_str(include_str!("server.config.json")).unwrap();
    println!("{}", serde_json::to_string_pretty(&config).unwrap());

    let tokens: Vec<String> = serde_json::from_value(
        config["capabilities"]["semanticTokensProvider"]["legend"]["tokenTypes"].clone(),
    )
    .unwrap();
    let tokens = to_enum_body(tokens);

    let modifiers: Vec<String> = serde_json::from_value(
        config["capabilities"]["semanticTokensProvider"]["legend"]["tokenModifiers"].clone(),
    )
    .unwrap();
    let modifiers = to_enum_body(modifiers);

    let mut file = fs::File::create(&dest_path).unwrap();
    writeln!(file, "pub enum Token {{ {tokens} }}").unwrap();
    writeln!(file, "pub enum Modifier {{{modifiers}}}").unwrap();
}
