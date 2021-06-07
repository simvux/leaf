use std::env;
use std::path::{Path, PathBuf};

const FILE_EXTENSION: &str = ".lm";

mod output;
pub use output::Output;
mod flags;

#[derive(Debug)]
pub struct Environment {
    pub luminapath: PathBuf,
    pub project_folder: PathBuf,
    pub entrypoint: String,
    pub output: Output,
    pub optimize: bool,
    pub panicky: bool,
}

impl Environment {
    pub fn test_env() -> Self {
        Self {
            project_folder: PathBuf::from("test-project-folder"),
            luminapath: PathBuf::from("test-lumina-path"),
            entrypoint: String::from("test-module"),
            output: Output::default(),
            optimize: true,
            panicky: true,
        }
    }

    pub fn from(entrypoint: String, project_folder: PathBuf, luminapath: PathBuf) -> Self {
        Self {
            project_folder,
            entrypoint,
            luminapath,
            optimize: true,
            panicky: false,
            output: Output::default(),
        }
    }

    pub fn discover<'a>() -> Result<Self, &'a str> {
        let args = env::args();
        if args.len() < 2 {
            Err("you need to provide a filename")
        } else {
            let mut args = env::args().collect::<Vec<String>>();
            let name = args.pop().unwrap();
            let path = PathBuf::from(&name);
            let name = name.trim_end_matches(FILE_EXTENSION).to_string();
            let current_dir = env::current_dir().expect("Could not get active directory");
            if !path.exists() {
                return Err("file does not exist");
            }
            let mut env = Environment::from(
                name,
                current_dir,
                env::var("LUMINA_PATH")
                    .map(|s| Path::new(&s).to_owned())
                    .unwrap_or_else(|_| {
                        env::current_dir().unwrap_or_else(|_| panic!("Could not find LUMINA_PATH"))
                    }),
            );

            // Skipping first since that's binary path
            env.parse_flags(args.drain(1..));

            Ok(env)
        }
    }
}
