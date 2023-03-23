mod build;
mod debloat;
mod util;
use std::env;
use std::error::Error;
use std::path::Path;

fn main() -> Result<(), Box<dyn Error>> {
    let args = env::args().collect::<Vec<_>>();
    assert_eq!(args.len(), 2);
    let project_dir = &args[1];
    let cur_dir = env::current_dir().unwrap().display().to_string();
    let main_file = build::build(project_dir);
    env::set_current_dir(Path::new(&format!("{}/{}", cur_dir, project_dir))).unwrap();
    debloat::debloat(&main_file)
}
