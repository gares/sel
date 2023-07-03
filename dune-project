(lang dune 3.5)

(name sel)

(generate_opam_files true)

(source
 (github gares/sel))

(authors "Enrico Tassi")

(maintainers "Enrico Tassi <enrico.tassi@inria.fr>")

(license MIT)

(package
 (name sel)
 (synopsis "Simple Event Library")
 (description "This library is the result of our experience in using threads and the Lwt async monad to tame the problem of writing a server which has to listen and react to multiple sources of events. The library itself is just sugar atop Unix.select. You can read more about the library on https://github.com/gares/sel")
 (depends
  (ocaml (>= 4.08))
  dune
  ppx_deriving
  (ppx_sexp_conv :with-test)
  (ppx_inline_test :with-test)
  (ppx_assert :with-test))
 (tags
  (event input output)))
