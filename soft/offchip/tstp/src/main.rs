use std::{fs::File, path::PathBuf};

use clap::{Parser, ValueHint};
use rustix::termios::{tcgetattr, tcsetattr, OptionalActions};

#[derive(Parser, Debug)]
#[command(version, author = "Alex Saveau (@SUPERCILEX)")]
#[command(infer_subcommands = true, infer_long_args = true)]
#[command(arg_required_else_help = true)]
#[command(max_term_width = 100)]
#[cfg_attr(test, command(help_expected = true))]
struct Baud {
    #[arg(value_hint = ValueHint::DirPath)]
    device: PathBuf,

    #[arg(short = 'b', long = "baud-rate")]
    baud_rate: u32,
}

fn main() {
    let Baud { device, baud_rate } = Baud::parse();

    let device = File::open(device).unwrap();

    let mut termios = tcgetattr(&device).unwrap();
    termios.make_raw();
    termios.set_speed(baud_rate).unwrap();
    tcsetattr(&device, OptionalActions::Drain, &termios).unwrap();
}
