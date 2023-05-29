(lang dune 3.6)

(name cse469)

(generate_opam_files true)

(source
 (github SUPERCILEX/hardcaml_riscv))

(authors "Alex Saveau")

(maintainers "Alex Saveau")

(license LICENSE)

(package
 (name cse469)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  base
  core
  core_unix
  dune
  hardcaml
  hardcaml_circuits
  hardcaml_verilator
  hardcaml_waveterm
  ocaml
  ocamlformat
  ppx_blob
  ppx_deriving_hardcaml
  ppx_expect
  ppx_jane)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
