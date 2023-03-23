use crate::util;
use colored::*;
use std::error::Error;
use std::fs::{self, File};
use std::io::Write;
use std::process::Command;

fn make_dirname(filename: &str, target_dirname: &str) -> String {
    let mut splitted = filename.split("/").collect::<Vec<_>>();
    let len = splitted.len() - 1;
    splitted[len] = target_dirname;
    splitted.join("/")
}

fn get_last_name(filename: &str) -> &str {
    let splitted = filename.split("/").collect::<Vec<_>>();
    let len = splitted.len() - 1;
    splitted[len]
}

fn copy_origin(filename: &str, dirname: &str) -> Result<String, Box<dyn Error>> {
    let tobe_copied = format!("{}/{}", dirname, get_last_name(filename));
    println!("Copied {} to {}", filename, &tobe_copied);
    fs::copy(filename, &tobe_copied)?;
    Ok(tobe_copied)
}

pub fn debloat(filename: &str) -> Result<(), Box<dyn Error>> {
    let temp_dirname = make_dirname(filename, "origin");
    fs::create_dir_all(&temp_dirname).unwrap();
    let diff_dirname = make_dirname(filename, "diff");
    fs::create_dir_all(&diff_dirname).unwrap();
    if filename.ends_with(".goir") {
        let copied_filename = copy_origin(filename, &temp_dirname)?;
        split_const_vars(filename)?;
        split_type_defs(filename)?;
        println!("Start debloating {filename}");
        let mut debloated = debloat_ssa_wrapper(filename)?;
        println!("Done debloat_ssa_wrapper.");
        debloated = debloat_package_and_func_sig(debloated)?;
        println!("Done debloat_package_and_func_sig.");
        debloated = debloat_func_debug_info(debloated)?;
        println!("Done debloat_func_debug_info.");
        debloated = debloat_rundefers(debloated)?;
        println!("Done debloat_rundefers.");
        debloated = debloat_locals(debloated)?;
        println!("Done debloat_locals.");
        debloated = align_free_vars(debloated)?;
        println!("Done align_free_vars.");
        debloated = remove_conseuctive_empty_lines(debloated)?;
        println!("Done remove_conseuctive_empty_lines.");
        debloated = remove_first_empty_lines(debloated)?;
        println!("Done remove_first_empty_lines.");
        write_debloated(filename, debloated)?;
        make_diff(filename, &copied_filename, &diff_dirname)?;
    }
    Ok(())
}

fn debloat_rundefers(debloated: String) -> Result<String, Box<dyn Error>> {
    let mut strs = vec![];
    for ir in debloated.split("\n") {
        if !ir.starts_with("\trundefers") {
            strs.push(ir);
        }
    }
    Ok(strs.join("\n"))
}

fn debloat_func_debug_info(debloated: String) -> Result<String, Box<dyn Error>> {
    let mut strs = vec![];
    for ir in debloated.split("\n") {
        if !ir.starts_with("\t; func") {
            strs.push(ir);
        }
    }
    Ok(strs.join("\n"))
}

fn make_diff(
    origin_filename: &str,
    copied_filename: &str,
    diff_dirname: &str,
) -> Result<(), Box<dyn Error>> {
    let diff_filename = format!("{}/diff_{}", diff_dirname, get_last_name(origin_filename));
    let diff_cmd = format!("diff {} {}", origin_filename, copied_filename);
    let diff_output = Command::new("sh")
        .arg("-c")
        .arg(diff_cmd)
        .output()
        .expect("failed to execute diff command");
    let mut output = File::create(&diff_filename)?;
    output.write_all(std::str::from_utf8(&diff_output.stdout).unwrap().as_bytes())?;
    Ok(())
}

fn write_debloated(filename: &str, debloated: String) -> Result<(), Box<dyn Error>> {
    let mut output = File::create(&filename)?;
    output.write_all(debloated.as_bytes())?;
    Ok(())
}

fn debloat_ssa_wrapper(filename: &str) -> Result<String, Box<dyn Error>> {
    let ir_str = util::read_file(filename)?;
    let mut synthetic = false;
    let mut strs: Vec<&str> = vec![];
    let mut temporals = vec![];
    for ir in ir_str.split("\n") {
        temporals.push(ir);
        if ir.starts_with("# Synthetic: wrapper for func") {
            synthetic = true;
        }
        if ir == "" && synthetic {
            synthetic = false;
            temporals = vec![];
        } else if ir == "" {
            strs.append(&mut temporals);
            temporals = vec![];
        }
    }
    Ok(strs.join("\n"))
}

fn debloat_package_and_func_sig(debloated: String) -> Result<String, Box<dyn Error>> {
    let mut strs = vec![];
    let mut prev_ir = "";
    for ir in debloated.split("\n") {
        if !prev_ir.starts_with("  var")
            && !ir.starts_with("package")
            && !ir.starts_with("  func")
            && !ir.starts_with("  var")
            && !ir.starts_with("  const")
            && !ir.starts_with("  type")
            && !ir.starts_with("    method")
        {
            strs.push(ir);
        }
        prev_ir = &ir;
    }
    Ok(strs.join("\n"))
}

fn debloat_locals(debloated: String) -> Result<String, Box<dyn Error>> {
    let mut strs = vec![];
    let mut locals = false;
    for ir in debloated.split("\n") {
        if ir.starts_with("# Locals") {
            locals = true;
        }
        if ir.starts_with("func") {
            locals = false;
        }
        if !locals {
            strs.push(ir);
        }
    }
    Ok(strs.join("\n"))
}

fn remove_first_empty_lines(debloated: String) -> Result<String, Box<dyn Error>> {
    let mut strs = vec![];
    let mut only_first_emptys = true;
    for ir in debloated.split("\n") {
        if ir != "" {
            only_first_emptys = false;
        }
        if !only_first_emptys {
            strs.push(ir);
        }
    }
    Ok(strs.join("\n"))
}

fn remove_conseuctive_empty_lines(debloated: String) -> Result<String, Box<dyn Error>> {
    let mut strs = vec![];
    let mut is_preceding_empty = false;
    for ir in debloated.split("\n") {
        if ir == "" {
            if !is_preceding_empty {
                strs.push(ir);
            }
            is_preceding_empty = true;
        } else {
            strs.push(ir);
            is_preceding_empty = false;
        }
    }
    Ok(strs.join("\n"))
}

fn split_interesting(
    filename: &str,
    output_file_name: &str,
    interesting_str: &str,
) -> Result<(), Box<dyn Error>> {
    let mut irs = vec![];
    let mut temporals = vec![];
    let ir_str = util::read_file(filename)?;
    for ir in ir_str.split("\n") {
        if ir.starts_with(interesting_str) || ir.starts_with("package") {
            temporals.push(ir);
        }
        if ir == "" {
            if temporals.len() > 1 {
                irs.append(&mut temporals);
            }
            temporals = vec![];
        }
    }
    let mut output = File::create(&output_file_name)?;
    output.write_all(irs.join("\n").as_bytes())?;
    Ok(())
}

fn split_const_vars(filename: &str) -> Result<(), Box<dyn Error>> {
    const OUTPUT_FILE_NAME: &str = "const.goir";
    split_interesting(filename, OUTPUT_FILE_NAME, "  const")?;
    println!(
        "{}",
        format!("Const variables written to {OUTPUT_FILE_NAME}").green()
    );
    Ok(())
}

fn split_type_defs(filename: &str) -> Result<(), Box<dyn Error>> {
    const OUTPUT_FILE_NAME: &str = "types.goir";
    split_interesting(filename, OUTPUT_FILE_NAME, "  type")?;
    println!(
        "{}",
        format!("Type definition written to {OUTPUT_FILE_NAME}").green()
    );
    Ok(())
}

fn align_free_vars(debloated: String) -> Result<String, Box<dyn Error>> {
    let mut strs = vec![];
    let mut free_var_appears = false;
    let mut free_vars = vec![];
    for ir in debloated.split("\n") {
        if free_var_appears && ir.starts_with("#   ") {
            free_vars.push(ir);
        } else if ir.starts_with("# Free variables:") {
            free_var_appears = true;
        } else if ir.starts_with("func ") && free_var_appears {
            let mut free_var_strs = "# Free variables: [".to_string();
            for free_var in &free_vars {
                let mut free_var = free_var.replace("\t", "");
                free_var = free_var.replace("#   ", "");
                free_var_strs = format!("{}{}, ", free_var_strs, free_var);
            }
            free_var_strs = format!("{}]", &free_var_strs[..free_var_strs.len() - 2]);
            strs.push(free_var_strs);
            strs.push(ir.to_string());
            free_vars = vec![];
        } else {
            free_var_appears = false;
            strs.push(ir.to_string());
        }
    }
    Ok(strs.join("\n"))
}
