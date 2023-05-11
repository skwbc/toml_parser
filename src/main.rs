use toml_parser::parse_toml;

fn main() {
    let input = r#"
    [package]
    name = "toml_parser"
    version = "0.1.0"
    edition = "2021"

    # See more keys and their definitions at https://doc.rust-lang.org/cargo/reference/manifest.html

    [dependencies]
    nom = "7.1.3"
    chrono = "0.4"
    maplit = "1.0"
    "#;

    let result = parse_toml(input);
    println!("{:#?}", result);
}
