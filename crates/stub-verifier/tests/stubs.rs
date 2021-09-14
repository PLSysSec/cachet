use std::fs::{read_dir, read_to_string};
use std::path::PathBuf;
use stub_verifier::stub::parse_stub;

#[test]
fn parse_all() -> Result<(), std::io::Error> {
    let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    d.push("tests/stubs");

    for stub in read_dir(d)? {
        let stub = stub?;
        let contents = read_to_string(stub.path())?;
        if let Err(e) = parse_stub(&contents) {
            panic!("Failed to parse: {:?}\n{}", stub.file_name(), e);
        }
    }
    Ok(())
}
