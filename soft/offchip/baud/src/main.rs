use std::{fs::File, path::PathBuf};

use clap::{Parser, ValueHint};
use clap2 as clap;
use rustix::termios::{tcgetattr2, tcsetattr2, OptionalActions, BOTHER, CBAUD};

#[derive(Parser, Debug)]
#[command(version, author = "Alex Saveau (@SUPERCILEX)")]
#[command(infer_subcommands = true, infer_long_args = true)]
#[command(disable_help_flag = true)]
#[command(arg_required_else_help = true)]
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
    let mut termios = tcgetattr2(&device).unwrap();
    termios.c_cflag &= !CBAUD;
    termios.c_cflag |= BOTHER;
    termios.c_ispeed = baud_rate;
    termios.c_ospeed = baud_rate;
    tcsetattr2(&device, OptionalActions::Drain, &termios).unwrap();
}
