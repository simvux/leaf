use super::Environment;

const VERSION: &str = "alpha-1.0";

#[allow(clippy::all)]
const ARGS: &[(&[&str], &str, fn(&mut Environment))] = &[
    (&["--help", "-h"], "show this message", |env| {
        env.output.help = true
    }),
    (&["--ast"], "dump the ast", |env| env.output.ast = true),
    (&["--hir"], "dump the hir", |env| env.output.hir = true),
    (&["--mir"], "dump the mir", |env| env.output.mir = true),
    (&["--dry"], "don't output any machine code", |env| {
        env.output.hir = true
    }),
];

impl Environment {
    pub fn parse_flags<I: Iterator<Item = String>>(&mut self, mut args: I) {
        loop {
            let arg = match args.next() {
                None => return,
                Some(arg) => arg,
            };

            for (flag, _, then_do) in ARGS {
                if flag.contains(&arg.as_str()) {
                    then_do(self)
                }
            }
        }
    }

    pub fn help_message(&self) {
        println!("lumina programming language {}\n", VERSION);

        for (flag, message, _) in ARGS {
            let flags = flag
                .iter()
                .map(|t| (*t).to_string())
                .collect::<Vec<_>>()
                .join("  ");
            println!(
                "  {}{}{}",
                flags,
                std::iter::repeat(" ")
                    .take(26 - flags.len())
                    .collect::<String>(),
                message
            );
        }
    }
}
