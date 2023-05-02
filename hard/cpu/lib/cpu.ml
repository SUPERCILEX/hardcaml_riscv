open! Core
open Hardcaml
module Uart = Uart
module Instruction = Instruction
module Memory_controller = Memory_controller

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; uart : 'a Uart.I.t [@rtlmangle true]
    }
  [@@deriving sexp_of, hardcaml]
end

module Counters = struct
  type 'a t =
    { cycles_since_boot : 'a [@bits 64]
    ; empty_alu_cycles : 'a [@bits 64]
    ; fetch_branch_target_buffer_hits : 'a [@bits 64]
    ; decode_branch_mispredictions : 'a [@bits 64]
    ; decode_jump_mispredictions : 'a [@bits 64]
    ; execute_branch_mispredictions : 'a [@bits 64]
    ; execute_jump_mispredictions : 'a [@bits 64]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { error : 'a
    ; uart : 'a Uart.O.t [@rtlmangle true]
    ; counters : 'a Counters.t
    }
  [@@deriving sexp_of, hardcaml]
end

let create scope ~bootloader { I.clock; clear; uart } =
  let open Signal in
  let open Pipeline in
  let ({ Memory_controller.I.clock = _
       ; load_instruction
       ; load = load_mem
       ; store = store_mem
       ; program_counter
       ; data_address
       ; data_size
       ; signed = signed_load
       ; write_data
       ; uart = _
       } as memory_controller_in)
    =
    { (Memory_controller.I.Of_signal.wires ()) with clock; uart }
  in
  let { Memory_controller.O.instruction = raw_instruction
      ; read_data
      ; error = mem_error
      ; uart
      ; stall_load_instruction
      ; stall_load = stall_mem_load
      ; stall_store = stall_mem_store
      }
    =
    Memory_controller.hierarchical
      scope
      ~bootloader:(String.to_list bootloader |> List.map ~f:Signal.of_char)
      memory_controller_in
  in
  let ({ Register_file.I.clock = _
       ; write_address = rd_address
       ; read_address1 = rs1_address
       ; read_address2 = rs2_address
       ; store = store_registers
       ; load = load_registers
       ; write_data = rd
       } as register_file_in)
    =
    { (Register_file.I.Of_signal.wires ()) with clock }
  in
  let { Register_file.O.rs1; rs2 } = Register_file.hierarchical scope register_file_in in
  let lock_pipeline = wire 1 -- "lock_pipeline" in
  let fetch_full = wire 1 in
  let ({ Fetch_instruction.I.clock = _
       ; clear = _
       ; pipeline_full = _
       ; stall_load_instruction = _
       ; jump
       ; jump_target
       ; control_flow_resolved_pc
       ; control_flow_resolved_jump_target
       ; control_flow_resolved_to_taken
       ; mem_error = _
       } as fetch_instruction_in)
    =
    { (Fetch_instruction.I.Of_signal.wires ()) with
      clock
    ; clear
    ; pipeline_full = fetch_full |: lock_pipeline
    ; stall_load_instruction
    ; mem_error
    }
  in
  let { Fetch_instruction.O.done_ = fetch_done
      ; load = load_instruction_
      ; program_counter = program_counter_
      ; has_prediction = has_fetch_prediction
      ; error = fetch_error
      }
    =
    Fetch_instruction.hierarchical scope fetch_instruction_in
  in
  load_instruction <== load_instruction_;
  program_counter <== program_counter_;
  let flush_pre_writeback = wire 1 -- "flush_pre_writeback" in
  let flush_pre_decode = wire 1 -- "flush_pre_decode" in
  let module Fetch_buffer =
    Stage_buffer
      (struct
        let capacity = 2
      end)
      (Decode_instruction.Data_in)
  in
  let fetch_consume = wire 1 in
  let fetch_head_update = Fetch_buffer.Entry.Of_signal.wires () in
  let fetch_write_data =
    { Decode_instruction.Data_in.raw_instruction = zero Parameters.word_width
    ; fetch_predicted_next_pc = zero Parameters.word_width
    ; has_fetch_prediction = gnd
    ; forward = { program_counter; error = fetch_error }
    }
  in
  let { Fetch_buffer.O.empty = _
      ; full = fetch_full_
      ; write_id = fetch_id
      ; all = _
      ; head = fetch_out
      }
    =
    Fetch_buffer.hierarchical
      scope
      ~name:"fetch_buffer"
      { Fetch_buffer.I.clock
      ; clear = clear |: flush_pre_writeback |: flush_pre_decode
      ; write_tail = { valid = fetch_done; ready = gnd; data = fetch_write_data }
      ; update = fetch_head_update
      ; pop = fetch_consume
      }
  in
  fetch_full <== fetch_full_;
  let decode_full = wire 1 in
  let { Decode_instruction.O.done_ = decode_done
      ; decoded
      ; predicted_next_pc = decode_predicted_next_pc
      ; jump = decode_jump
      ; is_branch = is_decode_branch_for_counters
      ; forward = decoder_forward
      }
    =
    Fetch_buffer.Entry.Of_signal.assign
      fetch_head_update
      { Fetch_buffer.Entry.id =
          fetch_id |> reg ~enable:fetch_done (Reg_spec.create ~clock ())
      ; raw =
          { valid = fetch_done |> reg (Reg_spec.create ~clock ~clear ())
          ; ready = vdd
          ; data =
              { (fetch_write_data
                 |> Decode_instruction.Data_in.Of_signal.reg
                      ~enable:fetch_done
                      (Reg_spec.create ~clock ()))
                with
                raw_instruction
              ; fetch_predicted_next_pc = program_counter
              ; has_fetch_prediction
              }
          }
      };
    let { Fetch_buffer.Entry.id = _; raw = { valid; ready; data } } = fetch_out in
    Decode_instruction.hierarchical
      scope
      { Decode_instruction.I.start = valid &: ready &: ~:decode_full &: ~:lock_pipeline
      ; data
      }
  in
  fetch_consume <== decode_done;
  let module Decode_buffer = Fast_fifo.Make (Load_registers.Data_in) in
  let decode_consume = wire 1 in
  let { Decode_buffer.O.rd_data = decode_out
      ; rd_valid = decode_outputs_valid
      ; full = decode_full_
      ; one_from_full = _
      }
    =
    Decode_buffer.hierarchical
      scope
      ~name:"decode_buffer"
      ~cut_through:false
      ~capacity:1
      { Decode_buffer.I.clock
      ; clear = clear |: flush_pre_writeback
      ; wr_data =
          (let { Decoder.O.instruction; rd; rs1; rs2; immediate } = decoded in
           let { Decode_instruction.Forward.program_counter; error } = decoder_forward in
           { Load_registers.Data_in.rs1_address = rs1
           ; rs2_address = rs2
           ; forward =
               { program_counter
               ; predicted_next_pc = decode_predicted_next_pc
               ; instruction
               ; rd_address = rd
               ; immediate
               ; error
               }
           })
      ; wr_enable = decode_done
      ; rd_enable = decode_consume
      }
  in
  decode_full <== decode_full_;
  let load_registers_full = wire 1 in
  let { Load_registers.O.done_ = load_registers_done
      ; load = load_registers_
      ; rs1_address = rs1_address_
      ; rs2_address = rs2_address_
      ; forward = load_registers_forward
      }
    =
    Load_registers.hierarchical
      scope
      { Load_registers.I.start =
          decode_outputs_valid &: ~:load_registers_full &: ~:lock_pipeline
      ; data = decode_out
      }
  in
  decode_consume <== load_registers_done;
  load_registers <== load_registers_;
  rs1_address <== rs1_address_;
  rs2_address <== rs2_address_;
  let module Load_registers_buffer =
    Stage_buffer
      (struct
        let capacity = 2
      end)
      (Execute.Data_in)
  in
  let load_registers_consume = wire 1 in
  let load_registers_head_update = Load_registers_buffer.Entry.Of_signal.wires () in
  let load_registers_write_data =
    let { Load_registers.Forward.program_counter
        ; predicted_next_pc
        ; instruction
        ; rd_address
        ; immediate
        ; error
        }
      =
      load_registers_forward
    in
    { Execute.Data_in.rs1_address
    ; rs2_address
    ; rs1 = zero Parameters.word_width
    ; rs2 = zero Parameters.word_width
    ; program_counter
    ; predicted_next_pc
    ; instruction
    ; immediate
    ; forward = { rd_address; error }
    }
  in
  let { Load_registers_buffer.O.empty = _
      ; full = load_registers_full_
      ; write_id = load_registers_id
      ; all = _
      ; head = load_registers_out
      }
    =
    Load_registers_buffer.hierarchical
      scope
      ~name:"load_regs_buffer"
      { Load_registers_buffer.I.clock
      ; clear = clear |: flush_pre_writeback
      ; write_tail =
          { valid = load_registers_done; ready = gnd; data = load_registers_write_data }
      ; update = load_registers_head_update
      ; pop = load_registers_consume
      }
  in
  load_registers_full <== load_registers_full_;
  let module Bypass_register = struct
    type 'a t =
      { rd_address : 'a [@bits 5]
      ; rd : 'a [@bits Parameters.word_width]
      }
    [@@deriving sexp_of, hardcaml]
  end
  in
  let module Bypass_buffer =
    Stage_buffer
      (struct
        let capacity = 3
      end)
      (Bypass_register)
  in
  let bypass_registers =
    List.init 3 ~f:(fun _ -> Bypass_buffer.Entry.Of_signal.wires ())
  in
  let execute_full = wire 1 in
  let alu_bypass_id = wire 2 in
  let { Execute.O.done_ = execute_done; data = execute_data; resolved_control_flow } =
    Load_registers_buffer.Entry.Of_signal.assign
      load_registers_head_update
      { Load_registers_buffer.Entry.id =
          load_registers_id |> reg ~enable:load_registers_done (Reg_spec.create ~clock ())
      ; raw =
          { valid = load_registers_done |> reg (Reg_spec.create ~clock ~clear ())
          ; ready = vdd
          ; data =
              { (load_registers_write_data
                 |> Execute.Data_in.Of_signal.reg
                      ~enable:load_registers_done
                      (Reg_spec.create ~clock ()))
                with
                rs1
              ; rs2
              }
          }
      };
    let { Load_registers_buffer.Entry.id = _; raw = { valid; ready; data } } =
      load_registers_out
    in
    let { Execute.Data_in.rs1_address; rs2_address; rs1; rs2; _ } = data in
    let module Bypass = struct
      type 'a t =
        { ready : 'a
        ; rd : 'a [@bits Parameters.word_width]
        }
      [@@deriving sexp_of, hardcaml]
    end
    in
    let bypass ~target_address ~default =
      List.map
        bypass_registers
        ~f:(fun
             { Bypass_buffer.Entry.id = _
             ; raw = { valid; ready; data = { rd_address; rd } }
             }
           ->
        { With_valid.valid = valid &: (rd_address ==: target_address)
        ; value = Bypass.Of_signal.pack { ready; rd }
        })
      |> priority_select_with_default
           ~default:(Bypass.Of_signal.pack { ready = vdd; rd = default })
      |> Bypass.Of_signal.unpack
    in
    let { Bypass.ready = rs1_ready; rd = rs1 } =
      bypass ~target_address:rs1_address ~default:rs1
    in
    let { Bypass.ready = rs2_ready; rd = rs2 } =
      bypass ~target_address:rs2_address ~default:rs2
    in
    let regs_ready =
      rs1_ready -- "execute_stage$rs1_ready" &: rs2_ready -- "execute_stage$rs2_ready"
    in
    Execute.hierarchical
      scope
      { Execute.I.clock
      ; clear = clear |: flush_pre_writeback
      ; start =
          (let debounce start =
             start
             &: (~:(start |> reg (Reg_spec.create ~clock ~clear ()))
                 |: (load_registers_consume |> reg (Reg_spec.create ~clock ~clear ())))
           in
           debounce (valid &: ready &: regs_ready &: ~:execute_full &: ~:lock_pipeline))
          |: (valid &: data.forward.error)
      ; bypass_id = alu_bypass_id
      ; data = { data with rs1; rs2 }
      }
  in
  let execute_done = execute_done &: ~:flush_pre_writeback in
  load_registers_consume <== execute_done;
  let is_execute_branch_for_counters =
    let ( valid
        , { Execute.Resolved_control_flow.jump = jump_
          ; jump_target = jump_target_
          ; program_counter
          ; taken
          ; resolved_jump_target
          ; is_branch
          } )
      =
      ( reg (Reg_spec.create ~clock ~clear ()) execute_done
      , Execute.Resolved_control_flow.Of_signal.reg
          ~enable:execute_done
          (Reg_spec.create ~clock ())
          resolved_control_flow )
    in
    flush_pre_writeback <== (jump_ &: valid);
    flush_pre_decode <== (decode_jump &: decode_done);
    jump <== (flush_pre_writeback |: flush_pre_decode);
    jump_target <== mux2 flush_pre_writeback jump_target_ decode_predicted_next_pc;
    control_flow_resolved_pc <== program_counter;
    control_flow_resolved_jump_target <== resolved_jump_target;
    control_flow_resolved_to_taken <== (valid &: taken);
    is_branch
  in
  let module Execute_buffer = Fast_fifo.Make (Execute.Data_out) in
  let writeback_done = wire 1 in
  let { Execute_buffer.O.rd_data = execute_out
      ; rd_valid = execute_outputs_valid
      ; full = execute_full_
      ; one_from_full = _
      }
    =
    Execute_buffer.hierarchical
      scope
      ~name:"execute_buffer"
      ~cut_through:false
      ~capacity:1
      { Execute_buffer.I.clock
      ; clear
      ; wr_data = execute_data
      ; wr_enable = execute_done
      ; rd_enable = writeback_done
      }
  in
  execute_full <== execute_full_;
  let { Load_memory_and_store.O.done_ = writeback_done_
      ; store_registers = store_registers_
      ; load_mem = load_mem_
      ; store_mem = store_mem_
      ; error
      }
    =
    let { Execute.Data_out.rd = rd_
        ; is_writeback_instruction
        ; is_load_instruction
        ; signed_load = signed_load_
        ; data_address = data_address_
        ; is_store_instruction
        ; store_data
        ; data_size = data_size_
        ; bypass_id = _
        ; forward = { rd_address = rd_address_; error }
        }
      =
      execute_out
    in
    rd <== mux2 is_load_instruction read_data rd_;
    signed_load <== signed_load_;
    data_address <== data_address_;
    write_data <== store_data;
    Memory_controller.Size.Binary.Of_signal.assign data_size data_size_;
    rd_address <== rd_address_;
    let debounce start =
      start
      &: (~:(start |> reg (Reg_spec.create ~clock ~clear ()))
          |: (writeback_done |> reg (Reg_spec.create ~clock ~clear ())))
    in
    Load_memory_and_store.hierarchical
      scope
      { Load_memory_and_store.I.clock
      ; clear
      ; start = debounce execute_outputs_valid &: ~:lock_pipeline
      ; is_writeback_instruction
      ; is_load_instruction
      ; stall_mem_load
      ; is_store_instruction
      ; stall_mem_store
      ; pipeline_error = error
      ; mem_error
      }
  in
  writeback_done <== writeback_done_;
  store_registers <== store_registers_;
  load_mem <== load_mem_;
  store_mem <== store_mem_;
  let error = reg_fb ~width:1 ~f:(error |> ( |: )) (Reg_spec.create ~clock ~clear ()) in
  lock_pipeline <== error;
  let bypass_buffer_full = wire 1 in
  let bypass_buffer_write = wire 1 in
  let { Bypass_buffer.O.empty = _
      ; full = bypass_buffer_full_
      ; write_id = alu_bypass_id_
      ; all = bypass_registers_
      ; head = _
      }
    =
    Bypass_buffer.hierarchical
      scope
      ~name:"execute_bypass_buffer"
      { Bypass_buffer.I.clock
      ; clear
      ; write_tail =
          (let { Execute.Data_out.rd
               ; is_writeback_instruction
               ; is_load_instruction
               ; forward = { rd_address; _ }
               ; _
               }
             =
             execute_data
           in
           bypass_buffer_write <== (execute_done &: is_writeback_instruction);
           { valid = bypass_buffer_write
           ; ready = ~:is_load_instruction
           ; data = { rd_address; rd }
           })
      ; update =
          (let { Execute.Data_out.is_load_instruction; bypass_id; _ } = execute_out in
           { id = bypass_id
           ; raw =
               { valid = store_registers &: is_load_instruction
               ; ready = vdd
               ; data = { rd_address; rd }
               }
           })
      ; pop = bypass_buffer_full &: bypass_buffer_write
      }
  in
  bypass_buffer_full <== bypass_buffer_full_;
  alu_bypass_id <== alu_bypass_id_;
  List.iter2_exn
    bypass_registers
    bypass_registers_
    ~f:Bypass_buffer.Entry.Of_signal.assign;
  { O.error
  ; uart
  ; counters =
      (let counter condition =
         reg_fb
           ~enable:~:lock_pipeline
           ~width:64
           ~f:(fun count -> mux2 condition (count +:. 1) count)
           (Reg_spec.create ~clock ~clear ())
       in
       { cycles_since_boot = counter vdd
       ; empty_alu_cycles =
           (let { Load_registers_buffer.Entry.id = _; raw = { valid; ready; data = _ } } =
              load_registers_out
            in
            counter ~:(valid &: ready))
       ; fetch_branch_target_buffer_hits = counter (fetch_done &: has_fetch_prediction)
       ; decode_branch_mispredictions =
           counter (is_decode_branch_for_counters &: flush_pre_decode)
       ; decode_jump_mispredictions =
           counter (~:is_decode_branch_for_counters &: flush_pre_decode)
       ; execute_branch_mispredictions =
           counter (is_execute_branch_for_counters &: flush_pre_writeback)
       ; execute_jump_mispredictions =
           counter (~:is_execute_branch_for_counters &: flush_pre_writeback)
       })
  }
;;

let hierarchical scope =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"cpu" (create ~bootloader:Parameters.bootloader_bytes)
;;
