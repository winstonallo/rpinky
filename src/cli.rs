#[derive(clap::Parser)]
pub struct Arguments {
    input_file: String,
}

impl Arguments {
    pub fn input_file(&self) -> &str {
        &self.input_file
    }
}
