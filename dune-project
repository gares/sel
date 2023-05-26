(lang dune 3.6)

(name sel)

(generate_opam_files true)

(source
 (github gares/sel))

(authors "Enrico Tassi")

(maintainers "Enrico Tassi")

(license LICENSE)

(package
 (name sel)
 (synopsis "Simple Event Library")
 (description "Some sugar atop Unix.select")
 (depends
  ocaml
  dune
  ppx_deriving
  (ppx_sexp_conv :with-test)
  (ppx_inline_test :with-test)
  (ppx_assert :with-test))
 (tags
  (event input output)))
