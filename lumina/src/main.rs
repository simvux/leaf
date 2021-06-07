#![feature(box_patterns)]
#![feature(try_trait)]
#![feature(print_internals)]
#![feature(option_unwrap_none)]
#![allow(dead_code)]
use colored::*;
use std::rc::Rc;

use lumina_compiler::verifier;
use lumina_env::Environment;
use lumina_util::Location;

pub const VERSION: &str = "alpha-1.0";

fn main() {
    let environment = match Environment::discover() {
        Ok(env) => Rc::new(env),
        Err(e) => {
            eprintln!("{}", e);
            return;
        }
    };
    if environment.output.help {
        environment.help_message();
        return;
    }

    match run(environment) {
        Ok(_main_returns) => {}
        Err(e) => eprintln!("{}: compilation failed\n\n{}", "lumina".green(), &e),
    }
}

fn run(env: Rc<Environment>) -> Result<(), Box<dyn std::error::Error>> {
    let entry = Rc::new(Location::from_string(format!("mod:{}", &env.entrypoint)).unwrap());

    if env.output.ast {
        println!("\n{}\n", " -- AST --".purple());
    }
    let collected = {
        let mut collected = lumina_compiler::collector::Storage::new();
        let mut collector =
            lumina_compiler::Collector::new(env.clone(), entry.clone(), &mut collected);
        collector.include()?;
        collected
    };

    if env.output.hir {
        println!("\n{}\n", " -- HIR --".purple());
    }

    // TODO: Here we want to attach the `list` and `closure` TID's
    let (errors, _verified) = {
        let settings = lumina_compiler::verifier::Settings::default();
        verifier::init_and_verify(env.clone(), settings, &collected)
    };

    for err in errors.iter() {
        // We probably want to collect the errors into an `dyn Error` and return it.
        println!("{}", err);
    }
    if !errors.is_empty() {
        return Ok(());
    }

    Ok(())
}

#[cfg(test)]
mod tests {}
