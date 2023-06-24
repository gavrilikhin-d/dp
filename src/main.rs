use std::io::Write;

use dp::{parser::Parser, Context};

fn main() {
    pretty_env_logger::init();

    miette::set_panic_hook();

    let mut context = Context::default();
    loop {
        print!(">>> ");
        std::io::stdout().flush().unwrap();

        let mut line = String::new();
        std::io::stdin().read_line(&mut line).unwrap();

        let root = context.find_rule("Root").unwrap();

        let res = root.parse(&line, &mut context);
        if let Err(syntax) = res {
            for err in syntax.errors() {
                println!(
                    "{:?}",
                    miette::Report::new(err.clone()).with_source_code(line.clone())
                );
            }
            continue;
        }
        let res = res.unwrap();
        let errors = res.syntax.errors().cloned().collect::<Vec<_>>();
        println!("{}", serde_json::to_string_pretty(&res.ast).unwrap());
        for err in errors {
            println!(
                "{:?}",
                miette::Report::new(err).with_source_code(line.clone())
            );
        }
    }
}
