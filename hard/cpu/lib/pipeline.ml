open! Core
open Hardcaml

module Fetch_instruction = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; pipeline_full : 'a
      ; stall_load_instruction : 'a
      ; jump : 'a
      ; jump_target : 'a [@bits Parameters.word_width]
      ; pending_return_address : 'a [@bits Parameters.word_width]
      ; predicted_branch_direction : 'a
      ; control_flow_resolved : 'a
      ; control_flow_resolved_pc : 'a [@bits Parameters.word_width]
      ; control_flow_resolved_jump_target : 'a [@bits Parameters.word_width]
      ; control_flow_resolved_branch_offset : 'a [@bits 12]
      ; control_flow_resolved_to_taken : 'a
      ; control_flow_resolved_is_branch : 'a
      ; control_flow_resolved_is_return : 'a
      ; mem_error : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { done_ : 'a
      ; load : 'a
      ; program_counter : 'a [@bits Parameters.word_width]
      ; has_prediction : 'a
      ; predicted_direction : 'a
      ; predicted_branch_target : 'a [@bits Parameters.word_width]
      ; error : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create
    scope
    { I.clock
    ; clear
    ; pipeline_full
    ; stall_load_instruction
    ; jump
    ; jump_target
    ; pending_return_address
    ; predicted_branch_direction
    ; control_flow_resolved
    ; control_flow_resolved_pc
    ; control_flow_resolved_jump_target
    ; control_flow_resolved_branch_offset
    ; control_flow_resolved_to_taken
    ; control_flow_resolved_is_branch
    ; control_flow_resolved_is_return
    ; mem_error
    }
    =
    let open Signal in
    let load = ~:pipeline_full &: ~:jump in
    let next_pc = wire Parameters.word_width in
    let { Branch_prediction.Jump_target_buffer.O.data =
            { target_pc = predicted_next_jump_pc; is_return }
        ; hit = has_jump_prediction
        }
      =
      Branch_prediction.Jump_target_buffer.hierarchical
        scope
        { clock
        ; load
        ; read_address = next_pc
        ; store = control_flow_resolved &: ~:control_flow_resolved_is_branch
        ; write_address = control_flow_resolved_pc
        ; write_data =
            { target_pc = control_flow_resolved_jump_target
            ; is_return = control_flow_resolved_is_return
            }
        }
    in
    let { Branch_prediction.Branch_target_buffer.O.data =
            { pc_offset = predicted_branch_pc_offset }
        ; hit = has_branch_prediction
        }
      =
      Branch_prediction.Branch_target_buffer.hierarchical
        scope
        { clock
        ; load
        ; read_address = next_pc
        ; store =
            control_flow_resolved
            &: control_flow_resolved_to_taken
            &: control_flow_resolved_is_branch
        ; write_address = control_flow_resolved_pc
        ; write_data = { pc_offset = control_flow_resolved_branch_offset }
        }
    in
    let done_ = load &: ~:stall_load_instruction in
    let current_pc = wire (width next_pc) in
    let has_prediction = has_jump_prediction |: has_branch_prediction in
    let predicted_next_branch_pc =
      (next_pc |> reg (Reg_spec.create ~clock ()))
      +: (predicted_branch_pc_offset @: gnd |> Fn.flip sresize (width current_pc))
    in
    next_pc
    <== mux2
          (has_prediction &: (done_ |> reg (Reg_spec.create ~clock ~clear ())))
          (mux2
             is_return
             pending_return_address
             (mux2
                has_branch_prediction
                (mux2 predicted_branch_direction predicted_next_branch_pc current_pc)
                predicted_next_jump_pc))
          current_pc;
    current_pc
    <== (mux2 jump jump_target (mux2 done_ (next_pc +:. 4) next_pc)
         |> reg
              (Reg_spec.override
                 (Reg_spec.create ~clock ~clear ())
                 ~clear_to:(of_int ~width:(width next_pc) Parameters.bootloader_start)));
    { O.done_
    ; load
    ; program_counter = next_pc
    ; has_prediction
    ; predicted_direction =
        has_branch_prediction &: predicted_branch_direction |: ~:has_branch_prediction
    ; predicted_branch_target =
        mux2 has_branch_prediction predicted_next_branch_pc predicted_next_jump_pc
    ; error = mem_error &: load
    }
  ;;

  let hierarchical scope =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"fetch_stage" create
  ;;
end

let is_branch instruction =
  let open Signal in
  [ Instruction.RV32I.Beq; Bne; Blt; Bge; Bltu; Bgeu ]
  |> List.map ~f:(fun op -> Instruction.All.Rv32i op, vdd)
  |> Instruction.Binary.Of_signal.match_ ~default:gnd instruction
;;

let is_link_reg id =
  let open Signal in
  id ==:. 1 |: (id ==:. 5)
;;

let is_return { Decoder.O.instruction; rd; rs1; rs2 = _; immediate = _ } =
  let open Signal in
  let jalr = Instruction.Binary.Of_signal.is instruction (Rv32i Jalr) in
  jalr
  &: (~:(is_link_reg rd)
      &: is_link_reg rs1
      |: (is_link_reg rd &: is_link_reg rs1 &: (rs1 <>: rd)))
;;

module Decode_instruction_and_load_registers = struct
  module Forward = struct
    type 'a t =
      { program_counter : 'a [@bits Parameters.word_width]
      ; error : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Data_in = struct
    type 'a t =
      { raw_instruction : 'a [@bits Parameters.word_width]
      ; fetch_predicted_next_pc : 'a [@bits Parameters.word_width]
      ; has_fetch_prediction : 'a
      ; fetch_predicted_direction : 'a
      ; fetch_predicted_branch_target : 'a [@bits Parameters.word_width]
      ; forward : 'a Forward.t [@rtlprefix "fi$"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; restore_from_retirement : 'a
      ; return_address_stack_entries_from_retirement : 'a
           [@bits Branch_prediction.Return_address_stack.Params.address_bits]
      ; data : 'a Data_in.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { done_ : 'a
      ; decoded : 'a Decoder.O.t
      ; predicted_next_pc : 'a [@bits Parameters.word_width]
      ; jump : 'a
      ; is_control_flow : 'a
      ; is_branch : 'a
      ; predicted_direction : 'a
      ; predicted_branch_target : 'a [@bits Parameters.word_width]
      ; has_fetch_prediction : 'a [@rtlsuffix "_forward"]
      ; forward : 'a Forward.t [@rtlprefix "fo$"]
      ; pending_return_address : 'a [@bits Parameters.word_width]
      ; return_address_stack_entries : 'a
           [@bits Branch_prediction.Return_address_stack.Params.address_bits]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create
    scope
    { I.clock
    ; clear
    ; start
    ; restore_from_retirement
    ; return_address_stack_entries_from_retirement
    ; data =
        { raw_instruction
        ; fetch_predicted_next_pc
        ; fetch_predicted_branch_target
        ; has_fetch_prediction
        ; fetch_predicted_direction
        ; forward = { program_counter; error } as forward
        }
    }
    =
    let open Signal in
    let ({ Decoder.O.instruction; rd; rs1; rs2 = _; immediate } as decoded) =
      Decoder.hierarchical scope { instruction = raw_instruction }
    in
    let is_branch = is_branch instruction in
    let jal = Instruction.Binary.Of_signal.is instruction (Rv32i Jal) in
    let jalr = Instruction.Binary.Of_signal.is instruction (Rv32i Jalr) in
    let is_return = is_return decoded in
    let { Branch_prediction.Return_address_stack.O.entries; data = { return_pc } } =
      let is_call =
        jal
        &: is_link_reg rd
        |: (jalr
            &: (is_link_reg rd
                &: ~:(is_link_reg rs1)
                |: (is_link_reg rd &: is_link_reg rs1)))
      in
      Branch_prediction.Return_address_stack.hierarchical
        scope
        { clock
        ; clear
        ; push = start &: is_call
        ; pop = start &: is_return
        ; set_num_entries = restore_from_retirement
        ; entries = return_address_stack_entries_from_retirement
        ; write_data = { return_pc = program_counter +:. 4 }
        }
    in
    let jump = is_branch &: (immediate <+. 0) |: jal |: (jalr &: is_return) in
    let jump_target = program_counter +: immediate in
    let predicted_next_pc =
      mux2
        has_fetch_prediction
        fetch_predicted_next_pc
        (mux2 jump (mux2 is_return return_pc jump_target) (program_counter +:. 4))
    in
    { O.done_ = start
    ; decoded
    ; predicted_next_pc
    ; predicted_branch_target =
        mux2 has_fetch_prediction fetch_predicted_branch_target jump_target
    ; jump = ~:error &: ~:has_fetch_prediction &: jump
    ; is_control_flow = is_branch |: jal |: jalr
    ; is_branch
    ; predicted_direction = mux2 has_fetch_prediction fetch_predicted_direction jump
    ; has_fetch_prediction
    ; forward =
        { forward with
          error =
            error |: (start &: Instruction.Binary.Of_signal.is instruction (Rv32i Invalid))
        }
    ; pending_return_address = return_pc
    ; return_address_stack_entries = entries
    }
  ;;

  let hierarchical scope =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"decode_and_load_regs_stage" create
  ;;
end

module Execute = struct
  module Forward = struct
    type 'a t =
      { rd_address : 'a [@bits 5]
      ; error : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Data_in = struct
    type 'a t =
      { rs1_address : 'a [@bits 5]
      ; rs2_address : 'a [@bits 5]
      ; rs1 : 'a [@bits Parameters.word_width]
      ; rs2 : 'a [@bits Parameters.word_width]
      ; program_counter : 'a [@bits Parameters.word_width]
      ; predicted_next_pc : 'a [@bits Parameters.word_width]
      ; instruction : 'a Instruction.Binary.t [@rtlmangle true]
      ; immediate : 'a [@bits 32]
      ; return_address_stack_entries : 'a
           [@bits Branch_prediction.Return_address_stack.Params.address_bits]
      ; forward : 'a Forward.t [@rtlprefix "fi$"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; bypass_id : 'a [@bits 2] [@rtlsuffix "_in"]
      ; data : 'a Data_in.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Data_out = struct
    type 'a t =
      { rd : 'a [@bits Parameters.word_width]
      ; is_writeback_instruction : 'a
      ; is_load_instruction : 'a
      ; signed_load : 'a
      ; data_address : 'a [@bits Parameters.word_width]
      ; is_store_instruction : 'a
      ; store_data : 'a [@bits Parameters.word_width]
      ; data_size : 'a Memory_controller.Size.Binary.t [@rtlmangle true]
      ; forward : 'a Forward.t [@rtlprefix "fo$"]
      ; bypass_id : 'a [@bits 2] [@rtlsuffix "_out"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Resolved_control_flow = struct
    type 'a t =
      { jump : 'a
      ; jump_target : 'a [@bits Parameters.word_width]
      ; program_counter : 'a [@bits Parameters.word_width] [@rtlsuffix "_out"]
      ; taken : 'a
      ; resolved_jump_target : 'a [@bits Parameters.word_width]
      ; resolved_branch_offset : 'a [@bits 12]
      ; is_control_flow : 'a
      ; is_branch : 'a
      ; is_return : 'a
      ; return_address_stack_entries : 'a
           [@bits Branch_prediction.Return_address_stack.Params.address_bits]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { done_ : 'a
      ; data : 'a Data_out.t
      ; resolved_control_flow : 'a Resolved_control_flow.t
           [@rtlprefix "resolved_control_flow$"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let is_store_regs instruction =
    let open Signal in
    [ [ Instruction.RV32I.Lui
      ; Auipc
      ; Jal
      ; Jalr
      ; Lb
      ; Lh
      ; Lw
      ; Lbu
      ; Lhu
      ; Addi
      ; Slti
      ; Sltiu
      ; Xori
      ; Ori
      ; Andi
      ; Slli
      ; Srli
      ; Srai
      ; Add
      ; Sub
      ; Sll
      ; Slt
      ; Sltu
      ; Xor
      ; Srl
      ; Sra
      ; Or
      ; And
      ]
      |> List.map ~f:(fun op -> Instruction.All.Rv32i op)
    ; [ Instruction.RV32M.Mul; Mulh; Mulhsu; Mulhu; Div; Divu; Rem; Remu ]
      |> List.map ~f:(fun op -> Instruction.All.Rv32m op)
    ]
    |> List.concat
    |> List.map ~f:(fun op -> op, vdd)
    |> Instruction.Binary.Of_signal.match_ ~default:gnd instruction
  ;;

  let is_load_mem instruction =
    let open Signal in
    [ Instruction.RV32I.Lb; Lh; Lw; Lbu; Lhu ]
    |> List.map ~f:(fun op -> Instruction.All.Rv32i op, vdd)
    |> Instruction.Binary.Of_signal.match_ ~default:gnd instruction
  ;;

  let is_store_mem instruction =
    let open Signal in
    [ Instruction.RV32I.Sb; Sh; Sw ]
    |> List.map ~f:(fun op -> Instruction.All.Rv32i op, vdd)
    |> Instruction.Binary.Of_signal.match_ ~default:gnd instruction
  ;;

  let is_signed_load instruction =
    let open Signal in
    [ Instruction.RV32I.Lb; Lh; Lw ]
    |> List.map ~f:(fun op -> Instruction.All.Rv32i op, vdd)
    |> Instruction.Binary.Of_signal.match_ ~default:gnd instruction
  ;;

  let is_jump instruction =
    let open Signal in
    [ Instruction.RV32I.Jal; Jalr ]
    |> List.map ~f:(fun op -> Instruction.All.Rv32i op, vdd)
    |> Instruction.Binary.Of_signal.match_ ~default:gnd instruction
  ;;

  let compute_data_size instruction =
    Instruction.Binary.Of_signal.match_
      ~default:Memory_controller.Size.(Binary.Of_signal.of_enum Byte |> Binary.to_raw)
      instruction
      ([ [ Instruction.RV32I.Lb; Lbu; Sb ], Memory_controller.Size.Enum.Byte
       ; [ Lh; Lhu; Sh ], Half_word
       ; [ Lw; Sw ], Word
       ]
       |> List.map ~f:(fun (instructions, s) -> List.map instructions ~f:(fun i -> i, s))
       |> List.concat
       |> List.map ~f:(fun (i, s) ->
            ( Instruction.All.Rv32i i
            , Memory_controller.Size.Binary.(Of_signal.of_enum s |> to_raw) )))
    |> Memory_controller.Size.Binary.Of_signal.of_raw
  ;;

  let create
    scope
    { I.clock
    ; clear
    ; start
    ; bypass_id
    ; data =
        { rs1_address
        ; rs2_address
        ; rs1
        ; rs2
        ; program_counter
        ; predicted_next_pc
        ; instruction
        ; immediate
        ; return_address_stack_entries
        ; forward = { rd_address; error = _ } as forward
        }
    }
    =
    let open Signal in
    let { Alu.O.rd; jump; jump_target; done_ } =
      Alu.hierarchical
        scope
        { clock; clear; start; pc = program_counter; instruction; rs1; rs2; immediate }
    in
    { O.done_
    ; data =
        { rd
        ; is_writeback_instruction = is_store_regs instruction &: (rd_address <>:. 0)
        ; is_load_instruction = is_load_mem instruction
        ; signed_load = is_signed_load instruction
        ; data_address = rs1 +: immediate
        ; is_store_instruction = is_store_mem instruction
        ; store_data = rs2
        ; data_size = compute_data_size instruction
        ; forward
        ; bypass_id
        }
    ; resolved_control_flow =
        (let is_branch = is_branch instruction in
         { jump = mux2 jump jump_target (program_counter +:. 4) <>: predicted_next_pc
         ; jump_target = mux2 jump jump_target (program_counter +:. 4)
         ; program_counter
         ; taken = jump
         ; resolved_jump_target = jump_target
         ; resolved_branch_offset = msbs immediate |> Fn.flip sel_bottom 12
         ; is_control_flow = is_branch |: is_jump instruction
         ; is_branch
         ; is_return =
             is_return
               { instruction
               ; rd = rd_address
               ; rs1 = rs1_address
               ; rs2 = rs2_address
               ; immediate
               }
         ; return_address_stack_entries
         })
    }
  ;;

  let hierarchical scope =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"execute_stage" create
  ;;
end

module Load_memory_and_store = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; is_writeback_instruction : 'a
      ; is_load_instruction : 'a
      ; stall_mem_load : 'a
      ; is_store_instruction : 'a
      ; stall_mem_store : 'a
      ; pipeline_error : 'a
      ; mem_error : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { done_ : 'a
      ; store_registers : 'a
      ; load_mem : 'a
      ; store_mem : 'a
      ; error : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create
    scope
    { I.clock
    ; clear
    ; start
    ; is_writeback_instruction
    ; is_load_instruction
    ; stall_mem_load
    ; is_store_instruction
    ; stall_mem_store
    ; pipeline_error
    ; mem_error
    }
    =
    let open Signal in
    let ( -- ) = Scope.naming scope in
    let ({ O.done_; store_registers; load_mem; store_mem; error } as out) =
      O.Of_always.wire zero
    in
    let running = Always.Variable.reg ~width:1 (Reg_spec.create ~clock ~clear ()) in
    running.value -- "running" |> ignore;
    let loading_mem =
      (load_mem.value
       |: (load_mem.value
           &: stall_mem_load
           |> reg ~enable:(start |: ~:stall_mem_load) (Reg_spec.create ~clock ~clear ()))
      )
      -- "loading_mem"
    in
    let storing_mem =
      (store_mem.value
       |: (store_mem.value
           &: stall_mem_store
           |> reg ~enable:(start |: ~:stall_mem_store) (Reg_spec.create ~clock ~clear ())
          ))
      -- "storing_mem"
    in
    Always.(
      compile
        [ if_
            start
            [ if_ is_load_instruction [ load_mem <-- vdd ]
              @@ elif is_writeback_instruction [ store_registers <-- vdd; done_ <-- vdd ]
              @@ elif is_store_instruction [] [ done_ <-- vdd ]
            ; when_
                is_store_instruction
                [ store_mem <-- vdd; done_ <-- ~:stall_mem_store ]
            ; when_ pipeline_error [ error <-- vdd ]
            ; running <-- ~:(done_.value)
            ]
          @@ elif
               running.value
               [ when_
                   (is_load_instruction &: ~:loading_mem)
                   [ store_registers <-- is_writeback_instruction; done_ <-- vdd ]
               ; when_ (is_store_instruction &: ~:stall_mem_store) [ done_ <-- vdd ]
               ; running <-- ~:(done_.value)
               ]
               []
        ; when_ (load_mem.value |: store_mem.value &: mem_error) [ error <-- vdd ]
        ]);
    let { O.done_; store_registers; load_mem = _; store_mem = _; error } =
      O.Of_always.value out
    in
    { O.done_
    ; store_registers = store_registers &: ~:pipeline_error
    ; load_mem = loading_mem &: ~:pipeline_error
    ; store_mem = storing_mem &: ~:pipeline_error
    ; error
    }
  ;;

  let hierarchical scope =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"writeback_stage" create
  ;;
end

module Stage_buffer (Params : sig
  val capacity : int
end)
(M : Interface.S) =
struct
  let id_width = Signal.address_bits_for Params.capacity

  module Raw_Entry = struct
    type 'a t =
      { valid : 'a
      ; ready : 'a
      ; data : 'a M.t [@rtlprefix "data$"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Entry = struct
    type 'a t =
      { id : 'a [@bits id_width]
      ; raw : 'a Raw_Entry.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; write_tail : 'a Raw_Entry.t [@rtlmangle true]
      ; update : 'a Entry.t [@rtlmangle true]
      ; pop : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { empty : 'a
      ; full : 'a
      ; write_id : 'a [@bits id_width]
      ; all : 'a Entry.t list [@length Params.capacity] [@rtlprefix "all$"]
      ; head : 'a Entry.t [@rtlmangle true]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create
    scope
    { I.clock
    ; clear
    ; write_tail = { valid = write; ready = _; data = _ } as write_entry
    ; update =
        { id = update_id; raw = { valid = update; ready = _; data = _ } } as update_entry
    ; pop
    }
    =
    let open Signal in
    let ( -- ) = Scope.naming scope in
    let entries_next = List.init Params.capacity ~f:(fun _ -> Entry.Of_signal.wires ()) in
    let entries =
      List.map entries_next ~f:(fun e ->
        let e_reg = e |> Entry.Of_signal.reg (Reg_spec.create ~clock ()) in
        { e_reg with
          raw =
            { e_reg.raw with
              valid = e.raw.valid |> reg (Reg_spec.create ~clock ~clear ())
            }
        })
    in
    let empty, full =
      let reduce_valid = List.map entries ~f:(fun e -> e.raw.valid) |> reduce in
      ~:(reduce_valid ~f:( |: )), reduce_valid ~f:( &: )
    in
    let write = (write &: (~:full |: pop)) -- "write_enable" in
    let pop = (pop &: ~:empty) -- "read_enable" in
    let next_id =
      reg_fb
        ~enable:write
        ~width:id_width
        ~f:(Fn.flip ( +:. ) 1)
        (Reg_spec.create ~clock ())
    in
    let update_override ({ Entry.id; raw = { valid; ready = _; data = _ } } as next) =
      Entry.Of_signal.mux2 (valid &: update &: (update_id ==: id)) update_entry next
    in
    let update_overrides = List.map ~f:update_override in
    let _assign =
      let entries =
        List.map entries ~f:(fun e -> e.raw.valid)
        |> Fn.flip List.zip_exn entries
        |> List.rev
        |> List.folding_map ~init:gnd ~f:(fun right (next_right, prev) ->
             ( next_right
             , Entry.Of_signal.mux2
                 pop
                 { prev with raw = { prev.raw with valid = right } }
                 prev ))
        |> List.rev
      in
      List.folding_map
        entries
        ~init:{ Entry.id = next_id; raw = write_entry }
        ~f:(fun left prev -> prev, Entry.Of_signal.mux2 write left prev)
      |> update_overrides
      |> List.iter2_exn entries_next ~f:Entry.Of_signal.assign;
      ()
    in
    { O.empty = empty |: clear
    ; full = full &: ~:clear
    ; write_id = next_id
    ; all = entries |> update_overrides
    ; head =
        (let head =
           List.rev entries_next
           |> List.reduce_exn ~f:(fun head tail ->
                Entry.Of_signal.mux2 head.raw.valid head tail)
         in
         let head_reg = head |> Entry.Of_signal.reg (Reg_spec.create ~clock ()) in
         { head_reg with
           raw =
             { head_reg.raw with
               valid = head.raw.valid |> reg (Reg_spec.create ~clock ~clear ())
             }
         }
         |> update_override)
    }
  ;;

  let hierarchical ~name scope =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name create
  ;;
end

module Tests = struct
  module Data = struct
    type 'a t = { test : 'a } [@@deriving sexp_of, hardcaml]
  end

  open
    Stage_buffer
      (struct
        let capacity = 2
      end)
      (Data)

  module Stage_buffer : sig end = struct
    let test_bench ~f (sim : (_ I.t, _ O.t) Cyclesim.t) =
      let open Bits in
      let inputs, outputs = Cyclesim.inputs sim, Cyclesim.outputs sim in
      let print_state () =
        Stdio.print_s
          [%message
            (inputs : Bits.t ref I.t)
              (outputs : Bits.t ref O.t)
              ~internals:(Cyclesim.internal_ports sim : (string * Bits.t ref) list)];
        Stdio.print_endline "";
        ()
      in
      let clear () =
        Cyclesim.reset sim;
        inputs.clear := vdd;
        Cyclesim.cycle sim;
        inputs.clear := gnd;
        ()
      in
      clear ();
      f print_state sim;
      ()
    ;;

    let sim ~f =
      let module Simulator = Cyclesim.With_interface (I) (O) in
      let scope = Scope.create ~flatten_design:true () in
      let sim = Simulator.create ~config:Cyclesim.Config.trace_all (create scope) in
      test_bench ~f sim;
      ()
    ;;

    let%expect_test "Simple" =
      sim ~f:(fun print_state sim ->
        let open Bits in
        let inputs = Cyclesim.inputs sim in
        let next () =
          Cyclesim.cycle sim;
          print_state ();
          ()
        in
        next ();
        inputs.pop := vdd;
        next ();
        inputs.write_tail.valid := vdd;
        inputs.write_tail.data.test := vdd;
        next ();
        inputs.write_tail.valid := gnd;
        next ();
        inputs.write_tail.valid := vdd;
        next ();
        next ();
        inputs.pop := gnd;
        next ();
        next ();
        inputs.write_tail.valid := gnd;
        inputs.pop := vdd;
        next ();
        next ();
        next ();
        ());
      [%expect
        {|
        ((inputs
          ((clock 0) (clear 0) (write_tail ((valid 0) (ready 0) (data ((test 0)))))
           (update ((id 0) (raw ((valid 0) (ready 0) (data ((test 0))))))) (pop 0)))
         (outputs
          ((empty 1) (full 0) (write_id 0)
           (all
            (((id 0) (raw ((valid 0) (ready 0) (data ((test 0))))))
             ((id 0) (raw ((valid 0) (ready 0) (data ((test 0))))))))
           (head ((id 0) (raw ((valid 0) (ready 0) (data ((test 0)))))))))
         (internals ((vdd 1) (gnd 0) (read_enable 0) (write_enable 0))))

        ((inputs
          ((clock 0) (clear 0) (write_tail ((valid 0) (ready 0) (data ((test 0)))))
           (update ((id 0) (raw ((valid 0) (ready 0) (data ((test 0))))))) (pop 1)))
         (outputs
          ((empty 1) (full 0) (write_id 0)
           (all
            (((id 0) (raw ((valid 0) (ready 0) (data ((test 0))))))
             ((id 0) (raw ((valid 0) (ready 0) (data ((test 0))))))))
           (head ((id 0) (raw ((valid 0) (ready 0) (data ((test 0)))))))))
         (internals ((vdd 1) (gnd 0) (read_enable 0) (write_enable 0))))

        ((inputs
          ((clock 0) (clear 0) (write_tail ((valid 1) (ready 0) (data ((test 1)))))
           (update ((id 0) (raw ((valid 0) (ready 0) (data ((test 0))))))) (pop 1)))
         (outputs
          ((empty 0) (full 0) (write_id 1)
           (all
            (((id 0) (raw ((valid 1) (ready 0) (data ((test 1))))))
             ((id 0) (raw ((valid 0) (ready 0) (data ((test 0))))))))
           (head ((id 0) (raw ((valid 1) (ready 0) (data ((test 1)))))))))
         (internals ((vdd 1) (gnd 0) (read_enable 0) (write_enable 1))))

        ((inputs
          ((clock 0) (clear 0) (write_tail ((valid 0) (ready 0) (data ((test 1)))))
           (update ((id 0) (raw ((valid 0) (ready 0) (data ((test 0))))))) (pop 1)))
         (outputs
          ((empty 1) (full 0) (write_id 1)
           (all
            (((id 0) (raw ((valid 0) (ready 0) (data ((test 1))))))
             ((id 0) (raw ((valid 0) (ready 0) (data ((test 0))))))))
           (head ((id 0) (raw ((valid 0) (ready 0) (data ((test 1)))))))))
         (internals ((vdd 1) (gnd 0) (read_enable 1) (write_enable 0))))

        ((inputs
          ((clock 0) (clear 0) (write_tail ((valid 1) (ready 0) (data ((test 1)))))
           (update ((id 0) (raw ((valid 0) (ready 0) (data ((test 0))))))) (pop 1)))
         (outputs
          ((empty 0) (full 0) (write_id 0)
           (all
            (((id 1) (raw ((valid 1) (ready 0) (data ((test 1))))))
             ((id 0) (raw ((valid 0) (ready 0) (data ((test 1))))))))
           (head ((id 1) (raw ((valid 1) (ready 0) (data ((test 1)))))))))
         (internals ((vdd 1) (gnd 0) (read_enable 0) (write_enable 1))))

        ((inputs
          ((clock 0) (clear 0) (write_tail ((valid 1) (ready 0) (data ((test 1)))))
           (update ((id 0) (raw ((valid 0) (ready 0) (data ((test 0))))))) (pop 1)))
         (outputs
          ((empty 0) (full 0) (write_id 1)
           (all
            (((id 0) (raw ((valid 1) (ready 0) (data ((test 1))))))
             ((id 1) (raw ((valid 0) (ready 0) (data ((test 1))))))))
           (head ((id 0) (raw ((valid 1) (ready 0) (data ((test 1)))))))))
         (internals ((vdd 1) (gnd 0) (read_enable 1) (write_enable 1))))

        ((inputs
          ((clock 0) (clear 0) (write_tail ((valid 1) (ready 0) (data ((test 1)))))
           (update ((id 0) (raw ((valid 0) (ready 0) (data ((test 0))))))) (pop 0)))
         (outputs
          ((empty 0) (full 1) (write_id 0)
           (all
            (((id 1) (raw ((valid 1) (ready 0) (data ((test 1))))))
             ((id 0) (raw ((valid 1) (ready 0) (data ((test 1))))))))
           (head ((id 0) (raw ((valid 1) (ready 0) (data ((test 1)))))))))
         (internals ((vdd 1) (gnd 0) (read_enable 0) (write_enable 1))))

        ((inputs
          ((clock 0) (clear 0) (write_tail ((valid 1) (ready 0) (data ((test 1)))))
           (update ((id 0) (raw ((valid 0) (ready 0) (data ((test 0))))))) (pop 0)))
         (outputs
          ((empty 0) (full 1) (write_id 0)
           (all
            (((id 1) (raw ((valid 1) (ready 0) (data ((test 1))))))
             ((id 0) (raw ((valid 1) (ready 0) (data ((test 1))))))))
           (head ((id 0) (raw ((valid 1) (ready 0) (data ((test 1)))))))))
         (internals ((vdd 1) (gnd 0) (read_enable 0) (write_enable 0))))

        ((inputs
          ((clock 0) (clear 0) (write_tail ((valid 0) (ready 0) (data ((test 1)))))
           (update ((id 0) (raw ((valid 0) (ready 0) (data ((test 0))))))) (pop 1)))
         (outputs
          ((empty 0) (full 0) (write_id 0)
           (all
            (((id 1) (raw ((valid 1) (ready 0) (data ((test 1))))))
             ((id 0) (raw ((valid 0) (ready 0) (data ((test 1))))))))
           (head ((id 1) (raw ((valid 1) (ready 0) (data ((test 1)))))))))
         (internals ((vdd 1) (gnd 0) (read_enable 1) (write_enable 0))))

        ((inputs
          ((clock 0) (clear 0) (write_tail ((valid 0) (ready 0) (data ((test 1)))))
           (update ((id 0) (raw ((valid 0) (ready 0) (data ((test 0))))))) (pop 1)))
         (outputs
          ((empty 1) (full 0) (write_id 0)
           (all
            (((id 1) (raw ((valid 0) (ready 0) (data ((test 1))))))
             ((id 0) (raw ((valid 0) (ready 0) (data ((test 1))))))))
           (head ((id 1) (raw ((valid 0) (ready 0) (data ((test 1)))))))))
         (internals ((vdd 1) (gnd 0) (read_enable 1) (write_enable 0))))

        ((inputs
          ((clock 0) (clear 0) (write_tail ((valid 0) (ready 0) (data ((test 1)))))
           (update ((id 0) (raw ((valid 0) (ready 0) (data ((test 0))))))) (pop 1)))
         (outputs
          ((empty 1) (full 0) (write_id 0)
           (all
            (((id 1) (raw ((valid 0) (ready 0) (data ((test 1))))))
             ((id 0) (raw ((valid 0) (ready 0) (data ((test 1))))))))
           (head ((id 1) (raw ((valid 0) (ready 0) (data ((test 1)))))))))
         (internals ((vdd 1) (gnd 0) (read_enable 0) (write_enable 0)))) |}]
    ;;

    let%expect_test "Update" =
      sim ~f:(fun print_state sim ->
        let open Bits in
        let inputs, outputs = Cyclesim.inputs sim, Cyclesim.outputs sim in
        inputs.pop := vdd;
        inputs.write_tail.valid := vdd;
        inputs.write_tail.data.test := vdd;
        inputs.update.id := !(outputs.write_id);
        Cyclesim.cycle sim;
        print_state ();
        inputs.write_tail.valid := gnd;
        inputs.update.raw.valid := vdd;
        inputs.update.raw.ready := vdd;
        print_state ();
        Cyclesim.cycle sim;
        print_state ();
        ());
      [%expect
        {|
        ((inputs
          ((clock 0) (clear 0) (write_tail ((valid 1) (ready 0) (data ((test 1)))))
           (update ((id 0) (raw ((valid 0) (ready 0) (data ((test 0))))))) (pop 1)))
         (outputs
          ((empty 0) (full 0) (write_id 1)
           (all
            (((id 0) (raw ((valid 1) (ready 0) (data ((test 1))))))
             ((id 0) (raw ((valid 0) (ready 0) (data ((test 0))))))))
           (head ((id 0) (raw ((valid 1) (ready 0) (data ((test 1)))))))))
         (internals ((vdd 1) (gnd 0) (read_enable 0) (write_enable 1))))

        ((inputs
          ((clock 0) (clear 0) (write_tail ((valid 0) (ready 0) (data ((test 1)))))
           (update ((id 0) (raw ((valid 1) (ready 1) (data ((test 0))))))) (pop 1)))
         (outputs
          ((empty 0) (full 0) (write_id 1)
           (all
            (((id 0) (raw ((valid 1) (ready 0) (data ((test 1))))))
             ((id 0) (raw ((valid 0) (ready 0) (data ((test 0))))))))
           (head ((id 0) (raw ((valid 1) (ready 0) (data ((test 1)))))))))
         (internals ((vdd 1) (gnd 0) (read_enable 0) (write_enable 1))))

        ((inputs
          ((clock 0) (clear 0) (write_tail ((valid 0) (ready 0) (data ((test 1)))))
           (update ((id 0) (raw ((valid 1) (ready 1) (data ((test 0))))))) (pop 1)))
         (outputs
          ((empty 1) (full 0) (write_id 1)
           (all
            (((id 0) (raw ((valid 0) (ready 0) (data ((test 1))))))
             ((id 0) (raw ((valid 0) (ready 0) (data ((test 0))))))))
           (head ((id 0) (raw ((valid 0) (ready 0) (data ((test 1)))))))))
         (internals ((vdd 1) (gnd 0) (read_enable 1) (write_enable 0)))) |}]
    ;;
  end
end
