use anyhow::Context;
use aunty::{StrongEntity, StrongObj};
use lulu::{
    frontend::token::tokenize,
    util::{diag::DiagnosticReporter, span::FileData},
};

fn main() -> anyhow::Result<()> {
    // Install services
    color_backtrace::install();

    // Read source files
    let in_path = std::env::args().nth(1).context("missing file argument")?;
    let in_data = std::fs::read_to_string(&in_path).context("failed to read source file")?;

    // Construct core services
    let diag = StrongObj::<DiagnosticReporter>::default();
    let file = StrongEntity::new().with_cyclic(|me, _| FileData {
        me,
        human_path: in_path,
        data: in_data,
    });

    // Tokenize source
    let tokens = tokenize(diag.downgrade(), &file.get::<FileData>());

    if diag.has_errors() {
        println!("Errors:");
        dbg!(diag);
    } else {
        println!("Success:");
        dbg!(tokens);
    }

    Ok(())
}
