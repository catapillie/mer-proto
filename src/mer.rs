use clap::Parser;
use colored::Colorize;
use std::{collections::HashMap, fs, process};

use cli::*;
use merlib::{
    binary,
    com::{self, AnalysisStage},
    diagnostics::{self, Severity},
    localization::{self, Lang},
    runtime::{Opcode, VM},
};

mod cli;
mod msg;

mod tests;

const DEFAULT_LANG: &dyn Lang = &localization::English;

fn main() {
    let cli = Cli::parse();
    match cli.command {
        Command::Com(args) => com(args),
        Command::Check(args) => check(args),
        Command::Run(args) => run(args),
        Command::Dis(args) => dis(args),
    }
}

fn com(args: Com) {
    let lang: &dyn Lang = match args.lang {
        Some(lang) => match lang {
            LangValue::En => &localization::English,
            LangValue::Fr => &localization::French,
        },
        None => match sys_locale::get_locale() {
            Some(locale) => match &locale[..2] {
                "en" => &localization::English,
                "fr" => &localization::French,
                _ => DEFAULT_LANG,
            },
            None => DEFAULT_LANG,
        },
    };

    let path_str = args.path.to_string_lossy();
    let source = match fs::read_to_string(&args.path) {
        Ok(source) => source,
        Err(e) => {
            msg::error(format!(
                "could not read file {}:\n      {}",
                path_str.bold(),
                e.to_string().italic()
            ));
            process::exit(e.raw_os_error().unwrap_or(1));
        }
    };

    let abt = match com::analyse_program(&source) {
        AnalysisStage::Ok(abt, diagnostics) => {
            let error_count = diagnostics
                .iter()
                .filter(|d| matches!(d.severity, Severity::Error))
                .count();
            let warning_count = diagnostics
                .iter()
                .filter(|d| matches!(d.severity, Severity::Warning))
                .count();
            if diagnostics.is_empty() {
                if !args.quiet {
                    msg::ok("analysis finished normally")
                }
            } else {
                msg::warn(format!(
                    "analysis finished with {} error(s) and {} warning(s)",
                    error_count.to_string().bold(),
                    warning_count.to_string().bold(),
                ));
                for diagnostic in diagnostics.into_iter() {
                    diagnostics::print_diagnostic(&path_str, &source, &diagnostic, lang);
                    println!();
                }
            }
            abt
        }
        AnalysisStage::CannotCompile(diagnostics) => {
            for diagnostic in diagnostics.into_iter() {
                diagnostics::print_diagnostic(&path_str, &source, &diagnostic, lang);
                println!();
            }
            msg::error("cannot compile with errors -- aborting");
            process::exit(1);
        }
    };

    let bytecode = match com::compile_to_bytecode(abt) {
        Ok(bytecode) => {
            if !args.quiet {
                msg::ok("bytecode generation finished normally");
            }
            bytecode
        }
        Err(err) => {
            msg::error(format!(
                "bytecode generation failed:\n    {}",
                err.to_string().italic(),
            ));
            process::exit(1);
        }
    };

    let out_path = match args.out {
        Some(path) => path,
        None => args.path.with_extension("out"),
    };

    let out_path_str = out_path.to_string_lossy();
    match com::write_bytecode(&out_path, &bytecode) {
        Ok(_) => {}
        Err(err) => {
            msg::error(format!(
                "something went wrong when writing bytecode into {}:\n{}",
                out_path_str.bold().underline(),
                err.to_string().italic(),
            ));
        }
    }

    if !args.quiet {
        msg::ok(format!(
            "compilation finished successfully to {}",
            out_path_str.bold().underline(),
        ));
    }

    if args.run {
        if !args.quiet {
            msg::info("running program now");
        }
        match VM::new(&bytecode).run::<()>() {
            Ok(()) => (),
            Err(error) => {
                msg::error(format!("run-time error: {}", error.to_string().bold()));
                process::exit(1);
            }
        }
    }

    process::exit(0);
}

fn check(args: Check) {
    let lang: &dyn Lang = match args.lang {
        Some(lang) => match lang {
            LangValue::En => &localization::English,
            LangValue::Fr => &localization::French,
        },
        None => match sys_locale::get_locale() {
            Some(locale) => match &locale[..2] {
                "en" => &localization::English,
                "fr" => &localization::French,
                _ => DEFAULT_LANG,
            },
            None => DEFAULT_LANG,
        },
    };

    let path_str = args.path.to_string_lossy();
    let source = match fs::read_to_string(&args.path) {
        Ok(source) => source,
        Err(e) => {
            msg::error(format!(
                "could not read file {}:\n      {}",
                path_str.bold(),
                e.to_string().italic()
            ));
            process::exit(e.raw_os_error().unwrap_or(1));
        }
    };

    match com::analyse_program(&source) {
        AnalysisStage::Ok(_, diagnostics) => {
            let error_count = diagnostics
                .iter()
                .filter(|d| matches!(d.severity, Severity::Error))
                .count();
            let warning_count = diagnostics
                .iter()
                .filter(|d| matches!(d.severity, Severity::Warning))
                .count();
            if diagnostics.is_empty() {
                msg::ok("analysis finished successfully with no diagnostics at all")
            } else {
                msg::warn(format!(
                    "analysis finished with {} error(s) and {} warning(s)",
                    error_count.to_string().bold(),
                    warning_count.to_string().bold(),
                ));
                for diagnostic in diagnostics.into_iter() {
                    diagnostics::print_diagnostic(&path_str, &source, &diagnostic, lang);
                    println!();
                }
                msg::info("warnings do not prevent compilation");
            }

            process::exit(0)
        }

        AnalysisStage::CannotCompile(diagnostics) => {
            for diagnostic in diagnostics.into_iter() {
                diagnostics::print_diagnostic(&path_str, &source, &diagnostic, lang);
                println!();
            }

            msg::info("this code will not compile");
            process::exit(1);
        }
    }
}

fn run(args: Run) {
    let path_str = args.path.to_string_lossy();
    let program = match fs::read(&args.path) {
        Ok(program) => program,
        Err(e) => {
            msg::error(format!(
                "could not read file {}:\n      {}",
                path_str.bold(),
                e.to_string().italic()
            ));
            process::exit(e.raw_os_error().unwrap_or(1));
        }
    };

    match VM::new(&program).run::<()>() {
        Ok(()) => (),
        Err(error) => {
            msg::error(format!("run-time error: {}", error.to_string().bold()));
            process::exit(1);
        }
    }
}

fn dis(args: Dis) {
    let path_str = args.path.to_string_lossy();
    let bytes = match fs::read(&args.path) {
        Ok(program) => program,
        Err(e) => {
            msg::error(format!(
                "could not read file {}:\n      {}",
                path_str.bold(),
                e.to_string().italic()
            ));
            process::exit(e.raw_os_error().unwrap_or(1));
        }
    };

    let opcodes = match binary::disassemble(&bytes) {
        Ok(opcodes) => opcodes,
        Err(err) => {
            msg::error(format!(
                "failed to disassemble program: {}",
                err.to_string().bold()
            ));
            process::exit(1);
        }
    };

    let mut map = HashMap::new();
    for (offset, opcode) in opcodes.iter() {
        map.insert(offset, opcode.clone());
    }

    println!("         ╥");
    for (offset, opcode) in opcodes.iter() {
        match opcode {
            Opcode::entry_point(addr) => {
                let Some(Opcode::function(name, _, _)) = map.get(&(*addr as u64)) else {
                    unreachable!()
                };
                println!(
                    "{offset:0>8} ╟─── !! entry-point -> {} [{addr:0>8}]",
                    name.bold()
                )
            }
            Opcode::function(name, param_count, local_count) => {
                println!(
                    "{offset:0>8} ║ :: {} ({} params, {} locals)",
                    name.bold(),
                    param_count.to_string().bold(),
                    local_count.to_string().bold()
                )
            }
            Opcode::call(addr) => {
                let Some(Opcode::function(name, _, _)) = map.get(&(*addr as u64)) else {
                    unreachable!()
                };
                println!(
                    "{offset:0>8} ║ {op:>20} -> {} [{addr:0>8}]",
                    name.bold(),
                    op = opcode.name(),
                )
            }
            Opcode::pop_n(n) | Opcode::dup_n(n) => println!(
                "{offset:0>8} ║ {op:>20} [{}]",
                n.to_string().bold(),
                op = opcode.name()
            ),
            Opcode::keep(at, n, len) => println!(
                "{offset:0>8} ║ {op:>20} [{}] of [{}] from {}",
                n.to_string().bold(),
                len.to_string().bold(),
                at.to_string().bold(),
                op = opcode.name()
            ),
            Opcode::keep_at(n, len) => println!(
                "{offset:0>8} ║ {op:>20} [{}] of [{}] from ...",
                n.to_string().bold(),
                len.to_string().bold(),
                op = opcode.name()
            ),
            Opcode::jmp(addr) | Opcode::jmp_if(addr) => println!(
                "{offset:0>8} ║ {op:>20} -> {}",
                format!("{addr:0>8}").bold(),
                op = opcode.name()
            ),
            Opcode::alloc_n(n) | Opcode::ld_heap_n(n) | Opcode::st_heap_n(n) => {
                println!(
                    "{offset:0>8} ║ {op:>20} [{}]",
                    n.to_string().bold(),
                    op = opcode.name(),
                )
            }
            Opcode::ld_loc(loc) | Opcode::st_loc(loc) => {
                println!(
                    "{offset:0>8} ║ {op:>20} {}",
                    loc.to_string().bold(),
                    op = opcode.name(),
                )
            }
            Opcode::ld_loc_n(loc, size) | Opcode::st_loc_n(loc, size) => {
                println!(
                    "{offset:0>8} ║ {op:>20} {} [{}]",
                    loc.to_string().bold(),
                    size.to_string().bold(),
                    op = opcode.name(),
                )
            }
            Opcode::realloc_loc(loc) => {
                println!(
                    "{offset:0>8} ║ {op:>20} {}",
                    loc.to_string().bold(),
                    op = opcode.name(),
                )
            }
            Opcode::ld_u8(num) => println!(
                "{offset:0>8} ║ {op:>20} {}",
                num.to_string().bold(),
                op = opcode.name()
            ),
            Opcode::ld_u16(num) => println!(
                "{offset:0>8} ║ {op:>20} {}",
                num.to_string().bold(),
                op = opcode.name()
            ),
            Opcode::ld_u32(num) => println!(
                "{offset:0>8} ║ {op:>20} {}",
                num.to_string().bold(),
                op = opcode.name()
            ),
            Opcode::ld_u64(num) => println!(
                "{offset:0>8} ║ {op:>20} {}",
                num.to_string().bold(),
                op = opcode.name()
            ),
            Opcode::ld_i8(num) => println!(
                "{offset:0>8} ║ {op:>20} {}",
                format!("{num:+}").bold(),
                op = opcode.name()
            ),
            Opcode::ld_i16(num) => println!(
                "{offset:0>8} ║ {op:>20} {}",
                format!("{num:+}").bold(),
                op = opcode.name()
            ),
            Opcode::ld_i32(num) => println!(
                "{offset:0>8} ║ {op:>20} {}",
                format!("{num:+}").bold(),
                op = opcode.name()
            ),
            Opcode::ld_i64(num) => println!(
                "{offset:0>8} ║ {op:>20} {}",
                format!("{num:+}").bold(),
                op = opcode.name()
            ),
            Opcode::ld_f32(num) => println!(
                "{offset:0>8} ║ {op:>20} {}",
                format!("{num:8.8}").bold(),
                op = opcode.name()
            ),
            Opcode::ld_f64(num) => println!(
                "{offset:0>8} ║ {op:>20} {}",
                format!("{num:8.8}").bold(),
                op = opcode.name()
            ),
            Opcode::dbg(ty)
            | Opcode::add(ty)
            | Opcode::sub(ty)
            | Opcode::mul(ty)
            | Opcode::div(ty)
            | Opcode::rem(ty)
            | Opcode::eq(ty)
            | Opcode::ne(ty)
            | Opcode::le(ty)
            | Opcode::lt(ty)
            | Opcode::ge(ty)
            | Opcode::gt(ty)
            | Opcode::bitand(ty)
            | Opcode::bitor(ty)
            | Opcode::bitxor(ty)
            | Opcode::neg(ty) => {
                println!(
                    "{offset:0>8} ║ {op:>20} <{}>",
                    ty.to_string().bold(),
                    op = opcode.name(),
                )
            }
            _ => {
                println!("{offset:0>8} ║ {op:>20}", op = opcode.name(),)
            }
        }
    }
    println!("         ╨");
}
