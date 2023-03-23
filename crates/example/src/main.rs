use grparser::parse;
use std::error::Error;
use std::fs::File;
use std::io::Read;

pub fn read_ir_file(file_path: &str) -> std::io::Result<String> {
    let mut file = File::open(file_path)?;
    let mut data = String::new();
    file.read_to_string(&mut data)?;
    Ok(data)
}

fn main() -> Result<(), Box<dyn Error>> {
    let ir_str = read_ir_file("../../example/project-example/main.goir")?;
    let parsed_instrs = parse::get_instrs(&ir_str);
    parse::dump_to_file("parsed-ir.txt", &parsed_instrs)?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::read_ir_file;
    use grparser::collection;
    use grparser::metadata;

    #[test]
    fn test_typ_lookup() {
        let types_str = read_ir_file("../../example/project-example/types.goir").unwrap();
        collection::parse_typ_defs(&types_str);

        let test_typ1 = "example/mypack.rect";
        let test_typ2 = "*net/http.Response";
        let test_typ3 = "example/mypack.status";

        println!("----------------------------------------------------");
        let rect_typ = collection::get_typ(test_typ1).unwrap();
        println!("{:?}", rect_typ);
        println!("{}", metadata::is_third_party_typ(&rect_typ));

        println!("----------------------------------------------------");
        let response_typ = collection::get_typ(test_typ2).unwrap();
        println!("{:?}", response_typ);
        println!("{}", metadata::is_third_party_typ(&response_typ));

        println!("----------------------------------------------------");
        let status_typ = collection::get_typ(test_typ3).unwrap();
        println!("{:?}", status_typ);
        println!("{}", metadata::is_third_party_typ(&status_typ));
    }
}
