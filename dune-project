(lang dune 2.9)
(name smt2key)
(generate_opam_files true)
(formatting)



(source (github wadoon/smt2key))
(license GPLv3)
(authors "Alexander Weigl")
(maintainers "weigl@kit.edu")


(package
 (name smt2key)
 (synopsis "xxx")
 (description "xxx")
 (depends
  (alcotest :with-test)
  (sexplib (>= v0.14.0))
  (pprint (>= 20220103))))
