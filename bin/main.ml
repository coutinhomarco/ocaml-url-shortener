open Lwt.Infix
open Url_shortener

let () =
  Lwt_main.run (
    User.initialize_db () >>= fun () ->
    let base_url = "http://localhost:8080" in
    create base_url >>= fun t ->
    Lwt.return (start_server t 8080)
  )