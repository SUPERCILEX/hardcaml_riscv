#import "@preview/polylux:0.3.1": *
#import themes.clean: *

#show: clean-theme.with(
  short-title: [Branch prediction and Hardcaml],
)

#set text(font: "Atkinson Hyperlegible", size: 23pt)
#set text(lang: "en")

#show link: it => text(fill: rgb("#0366d6"), it)
#show raw: set text(font: "Fira Code")
#set image(height: 85%)

#title-slide(
  authors: [Alex Saveau],
  title: [Branch prediction and Hardcaml],
  subtitle: link("https://github.com/SUPERCILEX/hardcaml_riscv")[SUPERCILEX/hardcaml_riscv],
  date: [May 2023],
)

#new-section-slide[Branch prediction: static]

#slide(title: [Participating stages])[
  - Fetch
  - Decode
  - Execute
]

#slide(title: [Execute])[
  #uncover("2-")[
  - Previous stages must pass their predicted next program counter along
    - Must be correct (as in the next instruction in the pipeline must be the predicted one)
    - It can be anything (security concerns aside)
    - Take advantage of low branch-to-instruction ratio to minimize storage cost by placing predicted pc in auxiliary SRAM instead of ROB
  ]
  - Verify prediction
]

#slide(title: [Decode])[
  #grid(rows: (auto, 1fr))[
    - Jump: address is hardcoded, always redirect
    - Jump relative: if a return, can make near perfect guess using return address stack
    - Branch: address is hardcoded, choose taken if points backwards, not taken if points forwards
  ][
    #align(bottom, pad(bottom: 1em, link("https://riscv.org/wp-content/uploads/2019/12/riscv-spec-20191213.pdf#page=39")[RISC-V control transfer instructions]))
  ]
]

#slide(title: [Sidenote: Call instruction codegen and dynamic languages])[
  Bad:
  ```
   11008:       auipc   ra, 0
   1100c:       jalr    ra, ra, 10
  ```

  - #link("https://www.sifive.com/blog/all-aboard-part-3-linker-relaxation-in-riscv-toolchain")[Linker relaxation] performs compile-time macro-op fusion
  - Virtual functions require `jalr`
  - Dynamic languages can optimize out if able to prove function will never be overridden (for example final class)
  - Rust does not enable relaxation #link("https://github.com/rust-embedded/wg/issues/361")[by default] #emoji.face.sad
]

#slide(title: [Return address stack (RAS)])[
  Perform updates in decode or fetch stage.

  On a call instruction, push to the stack.
  On a return, pop.

  What happens when the stack is full?
  Don't care!
  Prediction will be nonsense, but retirement will catch it.
]

#slide(title: [RAS — mispredictions #emoji.face.think])[
  - Cheap solution: keep track of RAS pointer, restore correct pointer on misprediction
    - Fails to handle pop #sym.arrow push sequence
    - Fixes tearing (e.g. we mispredicted a pop)
  - Full solution: replicate RAS and perform a direct FF #sym.arrow FF copy on misprediction
    - Typical RAS size for 32 64-bit addresses: 256 bytes, 2048 bits
]

#slide(title: [Fetch])[
  Can we do anything? Have access to the program counter...
]

#slide(title: [Branch target buffer (BTB)])[
  - Remember past jumps and branches
    - Only store taken branches
  - Repeat the last retirement action
  - Only update at retirement
    - Decode stage will do its thing
    - Don't know the real direction until retirement
    - Too large for FF copy
    - Indirection might work, but probably not worth it
]

#slide(title: [BTB contents])[
  - Memory size limited to single-cycle access latency
    - Need fetch prediction every cycle
  - Store target program counter
    - Maybe store relative offset to use fewer bits?
  - Store (partial) tag to avoid total misprediction failures
  - Is it a return instruction? Peek return address stack
  - Is it a jump? Always redirect
  - Is it a branch? Consult dynamic predictor

  #link("https://docs.boom-core.org/en/latest/sections/branch-prediction/index.html")[SonicBOOM predictor]
]

#slide(title: [Frontend design considerations])[
  Goal: keep backend *full* with *useful* instructions.

  Low power: use accuracy metric to stall when uncertain (e.g. `jalr`).
]

#new-section-slide[Branch prediction: dynamic]

#slide(title: [This talk: BATAGE])[
  The #link("https://inria.hal.science/hal-01799442/document")[paper] includes an analysis of prior art (including perceptron predictors).
]

#slide(title: [Last action predictors])[
  Fails on nested loops:
  ```rust
  repeat 10 {
    repeat 10 {
      // ...
    }
  }
  ```
]

#slide(title: [Bimodal predictors])[
  - Direction bit
  - Hysteresis bits
    - Definition: a physical property that lags behind changes in the effect causing it
]

#slide(title: [Bimodal predictors on nested loops])[
  #grid(columns: 3, rows: 2, column-gutter: 1em, row-gutter: 0.5em)[*Action*][*Direction*][*Hysteresis*][
    Before inner loop exit
  ][1][1][
    Exit inner loop
  ][1][0][
    Enter inner loop
  ][1][1]
]

#slide(title: [BAyesian (partially) TAgged GEometric predictor (BATAGE)])[
  - Base prediction handled by bimodal predictor
  - Array of bimodals each indexed by geometrically increasing history length
  - Allocate entry on misprediction (at retirement)
]

#slide(title: [Global history (GH)])[
  For every branch (and jump), store its outcome in a global history.
  Example: `1110111100000001000101011010`

  Banks are indexed by the program counter and this history.
]

#slide(title: [Consequences])[
  - Brilliant way to handle short loops
  - Correlates branch sequences
    - Example: `i % 2` in a loop
]

#slide(title: [GH — mispredictions])[
  - Need history to be accurate
    - Update in fetch, decode, and retirement stages
  - Unlike RAS, no pop
    - Keep head pointer in each stage, restore pointer from older stage on misprediction
    - Cascading chain of priorities (from retirement to fetch)
  - Typically \~1000 bits
]

#slide(title: [BATAGE bimodals])[
  - Store `num_takens` and `num_not_takens` as $N$ bit fields instead of direction/hysteresis
  - Tracks confidence: let $k = abs(#[`num_takens`] - #[`num_not_takens`])$
    - $k = 0$ #sym.arrow min confidence
    - $k = 2^N - 1$ #sym.arrow max confidence
  - Favor oldest high confidence predictions
]

#slide(title: [Extra resources])[
  - #link("http://csg.csail.mit.edu/6.823/lecnotes.html")[MIT computer architecture]
  - CMU computer architecture (content appears to have been scrubbed)
  - #link("https://courses.cs.washington.edu/courses/cse378/10au/lectures/Pentium4Arch.pdf")[Pentium architecture]
  - #link("https://ieeexplore.ieee.org/stamp/stamp.jsp?tp=&arnumber=7924286")[Skylake architecture]
  - #link("https://people.ece.ubc.ca/aamodt/papers/wwlfung.micro2007.pdf")[GPU thread coallessing]
  - #link("https://courses.cs.washington.edu/courses/csep548/06au/readings/ia-64.pdf")[IA-64 architecture]
  - #link("https://pages.cs.wisc.edu/~rajwar/papers/cpr_ieeemicro03.pdf")[OoO superscalar recovery]
]

#new-section-slide[Branch prediction: evaluation]

#slide(title: [Sample program jump mispredictions])[
  #image("jump-mispredictions.svg")
]

#slide(title: [Sample program branch mispredictions])[
  #image("branch-mispredictions.svg")
]

#slide(title: [Sample program branch prediction accuracy])[
  #image("branch-accuracy.svg")
]

#slide(title: [Sample program performance comparison])[
  #image("perf.svg")
]

#slide(title: [Resource costs])[
  - \~70 Kibs of memory
  - FFs: 2514 #sym.arrow 8748
  - LUTs: 7715 #sym.arrow 38397
  - MUXs: 1413 #sym.arrow 9779
]

#slide(title: [Random thoughts])[
  - Think of the children
    - Paper is great, but the code is hard to read
    - Use long variable names
    - Minimize spatial dependencies
    - Minimize global state
  - `for` loops with dependencies are a PitA to translate into hardware
  - Verification is challenging
]

#slide(title: [Random thoughts cont.])[
  - Compilers keep generating faster code!
    - Upgraded Rust twice during development and both times resulted in \~100s of saved clock cycles
  - Writing hardware is still hard
    - Cannot escape spatial dependencies
]

#new-section-slide[Hardcaml]

#slide(title: [But first, Chisel])[
  Chisel #link("https://github.com/chipsalliance/chisel/issues/1163")[does not validate port widths], so I dismissed it.

  Need to go back and see if there are good ideas to steal.
]

#slide(title: [Hardcaml core])[
  - Primitive operations (e.g. +, |, ^)
  - Wires
  - Registers
  - Muxes

  That's it!
]

#slide(title: [Think like the hardware])[
  #show raw: set text(size: 0.9em)
  ```ocaml
  (* Independent operations on every value are a transformation *)
  List.map values ~f:(fun value -> value +:. 1)

  (* Dependencies are a reduction *)
  reduce values ~f:(fun accumulator current -> {
      valid = accumulator.valid |: current.valid;
      value = mux2 accumulator.valid accumulator.value current.value
  })

  (* Trivial to use tree reduction now *)
  tree ~arity:2 values ~f:(...)
  ```
]

#slide(title: [No implicit behavior])[
  - No unassigned wires
  - No truncation/resizing
  - No guessing register vs. wire
  - No imperative constructs (like loops with dependencies)
  - No inferred memories
  - No inferred muxes\*
  - No if statements\*

  All of the above must be specified explicitly.
]

#slide(title: [Structs])[
  #show raw: set text(size: 0.9em)
  ```ocaml
  module Uart_out = struct
    type 'a t =
      { write_data : 'a [@bits 8]
      ; write_ready : 'a
      ; read_ready : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  out |> Uart_out.Of_signal.reg ~enable:valid
  Uart_out.Of_signal.mux2 condition uart1 uart2
  ```
]

#slide(title: [Higher order thinking])[
  #show raw: set text(size: 0.7em)
  ```ocaml
  let read_data activator =
    let activations = List.map segments ~f:activator in
    let any_active = tree ~arity:2 activations ~f:(reduce ~f:( |: )) in
    List.map2_exn activations segments ~f:(fun active (_, { read_data; _ }) ->
      { With_valid.valid = active |> reg ~enable:any_active (Reg_spec.create ~clock ())
      ; value = read_data
      })
    |> onehot_select

  let instruction = read_data (fun ({ load_instruction; _ }, _) -> load_instruction)
  let load_data = read_data (fun ({ load; _ }, _) -> load)

  let stall_load =
      List.map segments ~f:(fun (_, { stall_load; _ }) -> stall_load)
      |> tree ~arity:2 ~f:(reduce ~f:( |: ))
  ```
]

#slide(title: [\*Imperative logic])[
  ```ocaml
  if_
    (direction ==: resolved_direction)
    [ when_
        (hysteresis <:. hysteresis_max)
        [ next_hysteresis <-- hysteresis +:. 1 ]
    ]
  @@ elif
       (hysteresis >:. 0)
       [ next_hysteresis <-- hysteresis -:. 1 ]
       [ next_direction <-- resolved_direction ]
  ```
]

#slide(title: [Imperative logic cont.])[
  Just a library!
  Can look at its implementation.
]

#slide(title: [Integration with the system])[
  Easy! Just use Ocaml like a normal programming language to read/write files etc.
]

#focus-slide[Q&A]
