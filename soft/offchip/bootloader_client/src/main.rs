use std::{
    fs::File,
    io::{Read, Write},
    path::PathBuf,
};

use bootloader::{encode, Command};
use clap::{Args, Parser, Subcommand, ValueHint};
use clap2 as clap;

#[derive(Parser, Debug)]
#[command(version, author = "Alex Saveau (@SUPERCILEX)")]
#[command(infer_subcommands = true, infer_long_args = true)]
#[command(disable_help_flag = true)]
#[cfg_attr(test, command(help_expected = true))]
struct BootloaderClient {
    #[arg(value_hint = ValueHint::DirPath)]
    #[arg(short = 'd', long = "device")]
    device: PathBuf,

    #[command(subcommand)]
    cmd: Cmd,
}

#[derive(Subcommand, Debug)]
enum Cmd {
    Load(Load),
    Start(Start),
}

#[derive(Args, Debug)]
#[command(arg_required_else_help = true)]
struct Load {
    #[arg(value_hint = ValueHint::DirPath)]
    file: PathBuf,

    #[arg(short = 'a', long = "address")]
    #[arg(default_value = "1048576")]
    address: u64,
}

#[derive(Args, Debug)]
struct Start {
    args: Vec<String>,

    #[arg(short = 'a', long = "address")]
    #[arg(default_value = "1048576")]
    address: u64,
}

fn main() {
    let BootloaderClient { device, cmd } = BootloaderClient::parse();
    let mut device = File::options()
        .read(true)
        .write(true)
        .create(true)
        .open(device)
        .unwrap();

    match cmd {
        Cmd::Load(Load { file, address }) => {
            let buf = {
                let mut file = File::open(file).unwrap();

                let mut buf = Vec::new();
                file.read_to_end(&mut buf).unwrap();
                buf
            };

            device
                .write_all(
                    encode(Command::Load {
                        into_address: address,
                        len: u64::try_from(buf.len()).unwrap(),
                        checksum: buf.iter().copied().map(u64::from).sum(),
                    })
                    .unwrap()
                    .as_slice(),
                )
                .unwrap();

            device.write_all(&buf).unwrap();
        }
        Cmd::Start(Start { args, address }) => {
            let args = args.iter().map(String::as_str).collect::<Vec<_>>();
            device
                .write_all(
                    encode(Command::Start {
                        address,
                        args: args.as_slice(),
                    })
                    .unwrap()
                    .as_slice(),
                )
                .unwrap();
        }
    }
}
