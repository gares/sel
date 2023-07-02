(lang dune 3.5)

(name sel)

(generate_opam_files true)

(source
 (github gares/sel))

(authors "Enrico Tassi")

(maintainers "Enrico Tassi")

(license MIT)

(package
 (name sel)
 (synopsis "Simple Event Library")
 (description "Some sugar atop Unix.select")
 (depends
  (ocaml (>= 4.08))
  dune
  ppx_deriving
  (ppx_sexp_conv :with-test)
  (ppx_inline_test :with-test)
  (ppx_assert :with-test))
 (tags
  (event input output)))
