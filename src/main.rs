use anyhow::Context;
use aunty::{StrongEntity, StrongObj};
use lulu::{
    frontend::{parser::parse_file, token::tokenize},
    util::{diag::DiagnosticReporter, span::FileData},
};

fn main() -> anyhow::Result<()> {
    // Install services
    color_backtrace::install();
    env_logger::init_from_env(env_logger::Env::default().default_filter_or("info"));

    // Read source files
    let in_path = std::env::args().nth(1).context("missing file argument")?;
    let in_data = std::fs::read_to_string(&in_path).context("failed to read source file")?;

    // Construct core services
    let diag = StrongObj::<DiagnosticReporter>::default();
    let file = StrongEntity::new().with_cyclic(FileData::new(in_path, in_data));

    // Parse source
    let tokens = tokenize(diag.downgrade(), &file.get::<FileData>());
    let ast = parse_file(diag.downgrade(), &tokens);

    if diag.has_errors() {
        println!("Errors:");
        dbg!(diag);
    } else {
        println!("Success:");
        dbg!(ast);
    }

    Ok(())
}
