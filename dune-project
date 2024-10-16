(lang dune 3.16)
(name ocamlcompiler)
(generate_opam_files true)

(source
(github joaopaulo-creator/ocaml_compiler))
(authors "Joao Paulo")
(maintainers "Joao Paulo")
(license MIT)

(documentation https://url/to/documentation)

(package
 (name ocamlcompiler)

 (description "A longer description")
 (depends ocaml dune menhir ounit2)

 (tags
  (topics "to describe" your project)))

(using menhir 3.0)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
