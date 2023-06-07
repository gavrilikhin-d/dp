use std::io::Write;

use dp::{parsers::Parser, Context};

fn main() {
    miette::set_panic_hook();

    let mut context = Context::default();
    loop {
        print!(">>> ");
        std::io::stdout().flush().unwrap();

        let mut line = String::new();
        std::io::stdin().read_line(&mut line).unwrap();

        let root = context.find_rule("Root").unwrap();

        let res = root.parse(&line, &mut context);
        if let Err(err) = res {
            println!(
                "{:?}",
                miette::Report::new(err.clone()).with_source_code(line.clone())
            );
            continue;
        }
        println!(
            "{}",
            serde_json::to_string_pretty(&res.unwrap().ast).unwrap()
        );
    }
}
