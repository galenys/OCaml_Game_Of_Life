(lang dune 3.6)

(name game_of_life)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Shiv Bhatia")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name game_of_life)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
