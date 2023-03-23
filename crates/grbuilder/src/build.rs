use crate::util;
use colored::*;
use std::env;
use std::fs::OpenOptions;
use std::fs::{self, metadata};
use std::io::Write;
use std::path::Path;
use std::process::Command;

const GO_EXT: &'static str = "go";

fn traverse_dirs(dir: &str) -> Vec<String> {
    let mut file_paths = vec![];
    let paths = fs::read_dir(dir).unwrap();
    for path in paths {
        let path = path.unwrap().path();
        let path_str = path.to_str().unwrap();
        if metadata(&path_str).unwrap().is_dir() {
            file_paths.append(&mut traverse_dirs(&path_str));
        } else {
            if let Some(ext) = path.extension() {
                if ext == GO_EXT {
                    let file_path = fs::canonicalize(path_str).unwrap();
                    let file_path = file_path.to_str().unwrap();
                    file_paths.push(file_path.to_string());
                }
            }
        }
    }
    file_paths.iter().map(|s| s.into()).collect()
}

fn convert2ssa(file_paths: &Vec<String>) -> String {
    let build_cmd = "ssadump -build=SFDGPNE .";
    let mut ir_file_paths = vec![];
    let mut main_file = "".to_string();
    // 1. build to ssa
    for file_path in file_paths {
        let file_path_vec = file_path.split("/").collect::<Vec<_>>();
        let dir = file_path_vec[..file_path_vec.len() - 1].join("/").clone();
        let file_name = file_path_vec[file_path_vec.len() - 1];
        let goir_file_name = file_name.replace(GO_EXT, "goir");
        let build_cmd_with_file = format!("{} > {}", build_cmd, goir_file_name);
        env::set_current_dir(Path::new(&dir)).unwrap();
        Command::new("sh")
            .arg("-c")
            .arg(build_cmd_with_file)
            .output()
            .expect("failed to run ssadump");

        let ir_file_path = format!("{}/{}", dir, goir_file_name);
        if file_name != "main.go" {
            ir_file_paths.push(ir_file_path);
        } else {
            main_file = ir_file_path;
        }
        println!("{}", format!("Compiled {} to SSA form", file_name).yellow());
    }
    // 2. append generated ssa files to single ssa file (`main.goir`)
    assert_eq!(main_file.ends_with("main.goir"), true);
    let mut aggregated = OpenOptions::new()
        .write(true)
        .append(true)
        .open(&main_file)
        .unwrap();
    for ir_file_path in ir_file_paths {
        let file_content = util::read_file(&ir_file_path).unwrap();
        aggregated.write_all(file_content.as_bytes()).unwrap();
        println!(
            "{}",
            format!("Appended {} to {}", ir_file_path, main_file).blue()
        );
    }
    main_file
}

pub fn build(dir: &str) -> String {
    let go_files = traverse_dirs(dir);
    convert2ssa(&go_files)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::debloat;
    use std::fs;
    use std::path::Path;
    extern crate fs_extra;

    fn copy_project_dir(input_dir: &str, output_dir: &str) {
        let options = fs_extra::dir::CopyOptions::new();
        fs_extra::dir::copy(input_dir, output_dir, &options).unwrap();
    }

    fn remove_test_dir(dir: &str) {
        fs::remove_dir_all(dir).unwrap();
    }

    fn diff_test(file1: &str, file2: &str, copied_dir: &str) {
        let cmd = Command::new("/usr/bin/diff")
            .arg(file1)
            .arg(file2)
            .output()
            .unwrap();
        let stderr_str = String::from_utf8_lossy(&cmd.stderr);
        if stderr_str != "" {
            println!("diff command error: {} ", stderr_str.red());
            remove_test_dir(copied_dir);
            std::process::exit(1);
        }
        let output_str = String::from_utf8_lossy(&cmd.stdout);
        for s in output_str.split("\n") {
            if s.starts_with(">") || s.starts_with("<") {
                if !(&s[4..]).starts_with("Location:") && !s.contains("-->") {
                    println!("diff {}", output_str);
                    println!("{} ", "found diff. program exit.".red());
                    remove_test_dir(copied_dir);
                    std::process::exit(1);
                }
            }
        }
    }

    #[test]
    fn test_build() {
        let cur_dir = env::current_dir().unwrap().display().to_string();
        let origin_example_dir = "../../example/project-example";
        let copied_example_dir = "tests";
        let copied_example_dir_full_path =
            format!("{}/{}/{}", cur_dir, copied_example_dir, "project-example");
        copy_project_dir(origin_example_dir, copied_example_dir);
        let main_file = build(copied_example_dir);
        debloat::debloat(&main_file).unwrap();

        env::set_current_dir(Path::new(&cur_dir)).unwrap();
        diff_test(
            "tests/fixture/main.goir",
            &format!("{}/{}", copied_example_dir_full_path, "main.goir"),
            &copied_example_dir_full_path,
        );
        diff_test(
            "tests/fixture/const.goir",
            &format!("{}/{}", copied_example_dir_full_path, "const.goir"),
            &copied_example_dir_full_path,
        );
        diff_test(
            "tests/fixture/types.goir",
            &format!("{}/{}", copied_example_dir_full_path, "types.goir"),
            &copied_example_dir_full_path,
        );
        remove_test_dir(&copied_example_dir_full_path);
    }
}
