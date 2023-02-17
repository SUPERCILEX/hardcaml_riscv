set_param board.repoPaths "boards/"
set output_dir "outputs/"

set board [lindex $argv 0]

switch $board {
    "arty-a7-35" {
        puts "Setting up project for Arty A7 35"
        create_project -in_memory -part xc7a35ticsg324-1L
        set_property board_part digilentinc.com:arty-a7-35:part0:1.1 [current_project]
    }
    "arty-a7-100" {
        puts "Setting up project for Arty A7 100"
        create_project -in_memory -part xc7a100tcsg324-1
        set_property board_part digilentinc.com:arty-a7-100:part0:1.1 [current_project]
    }
    default {
        puts "Provide valid BOARD parameter. E.g.: BOARD={arty-a7-35|arty-a7-100}"
        exit 1
    }
}
