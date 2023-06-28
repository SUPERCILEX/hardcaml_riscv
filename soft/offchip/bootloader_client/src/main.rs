use std::{
    fs::File,
    io,
    io::{Read, Write},
    path::PathBuf,
    slice,
};

use bootloader::{encode, Command};
use clap::{Args, Parser, Subcommand, ValueHint};
use shared::PACKET_SIZE;

#[derive(Parser, Debug)]
#[command(version, author = "Alex Saveau (@SUPERCILEX)")]
#[command(infer_subcommands = true, infer_long_args = true)]
#[command(max_term_width = 100)]
#[cfg_attr(test, command(help_expected = true))]
struct BootloaderClient {
    #[arg(value_hint = ValueHint::DirPath)]
    #[arg(short = 'o', long = "output")]
    output: PathBuf,

    #[arg(value_hint = ValueHint::DirPath)]
    #[arg(short = 'i', long = "input")]
    input: PathBuf,

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
    let BootloaderClient { output, input, cmd } = BootloaderClient::parse();
    let mut device = FlowProtocol {
        device_out: File::options()
            .write(true)
            .append(true)
            .create(true)
            .open(output)
            .unwrap(),
        device_in: File::open(input).unwrap(),
    };

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

struct FlowProtocol {
    device_out: File,
    device_in: File,
}

impl Write for FlowProtocol {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        loop {
            let mut ack = 0;
            if self.device_in.read(slice::from_mut(&mut ack))? != 1 {
                return Err(io::Error::new(io::ErrorKind::Other, "No start ACK"));
            }
            if ack == 0xFF {
                break;
            }
        }

        let mut written = 0;
        for packet in buf.chunks(PACKET_SIZE) {
            self.device_out.write_all(packet)?;
            if packet.len() < PACKET_SIZE {
                self.device_out
                    .write_all(&[0; PACKET_SIZE][packet.len()..])?;
            }

            {
                let mut ack = 0;
                assert_eq!(
                    self.device_in.read(slice::from_mut(&mut ack))?,
                    1,
                    "Failed to receive ACK"
                );
                assert_eq!(ack, 0xFF, "Invalid ACK");
            }

            written += packet.len();
        }
        Ok(written)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.device_out.flush()
    }
}
