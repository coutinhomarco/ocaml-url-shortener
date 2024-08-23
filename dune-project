(lang dune 2.9)
(name url_shortener)

(generate_opam_files true)

(package
 (name url_shortener)
 (synopsis "A simple URL shortener")
 (description "An OCaml-based URL shortener with user authentication")
 (depends
  (ocaml (>= 4.08))
  (dune (>= 2.9))
  lwt
  lwt_ppx
  cohttp-lwt-unix
  caqti
  caqti-lwt
  caqti-driver-sqlite3
  yojson
  argon2
  jose
  uri))