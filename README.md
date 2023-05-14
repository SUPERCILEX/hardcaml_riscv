# Hardcaml RISC-V

An RV32IM processor written in [Hardcaml](https://github.com/janestreet/hardcaml).

The project is organized as follows:

- `hard` — All things related to the hardware implementation
    - `arty` — Build tooling and top module for the Arty A7-100T FPGA
    - `cpu` — Hardware agnostic CPU implementation
        - `bin` — Simulation, verification, and circuit compilation tooling
        - `lib` — CPU implementation
- `soft` — Software to use the CPU
    - `bootloader` — Shared library for the bootloader
    - `offchip` — Utilities for interacting with the CPU from the host operating system
        - `bootloader_client` — Bootloader command interface
    - `onchip` — Code meant to run on the CPU
        - `bootloader_server` — The bootloader server that runs on the CPU and waits for commands
          from the host
        - Various test programs that can be loaded by the bootloader to run on the CPU
    - `slib` — A standard library for programs running on the CPU
