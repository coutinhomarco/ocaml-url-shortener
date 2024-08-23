open Lwt
open Cohttp_lwt_unix
open Lwt.Infix

module UrlMap = Map.Make(String)

type url_record = {
  original_url: string;
  short_code: string;
  access_count: int ref;
  owner: string;
}

type t = {
  urls: url_record UrlMap.t ref;
  base_url: string;
  db: (module Caqti_lwt.CONNECTION);
}

let create base_url =
  User.get_db_connection () >>= function
  | Ok db -> Lwt.return { urls = ref UrlMap.empty; base_url; db }
  | Error _ -> Lwt.fail_with "Failed to connect to database"

let generate_short_code () =
  let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" in
  let code_length = 6 in
  String.init code_length (fun _ -> chars.[Random.int (String.length chars)])

let shorten_url t original_url owner =
  let short_code = generate_short_code () in
  let record = { original_url; short_code; access_count = ref 0; owner } in
  t.urls := UrlMap.add short_code record !(t.urls);
  t.base_url ^ "/" ^ short_code

let get_original_url t short_code =
  match UrlMap.find_opt short_code !(t.urls) with
  | Some record ->
      incr record.access_count;
      Some record.original_url
  | None -> None

let json_of_string s = `String s

let handle_register t body =
  body |> Cohttp_lwt.Body.to_string >>= fun body ->
  let json = Yojson.Safe.from_string body in
  let username = json |> Yojson.Safe.Util.member "username" |> Yojson.Safe.Util.to_string in
  let password = json |> Yojson.Safe.Util.member "password" |> Yojson.Safe.Util.to_string in
  User.create_user t.db ~username ~password >>= function
  | Ok () -> Server.respond_string ~status:`Created ~body:"User created successfully" ()
  | Error _ -> Server.respond_string ~status:`Bad_request ~body:"Failed to create user" ()

let handle_login t body =
  body |> Cohttp_lwt.Body.to_string >>= fun body ->
  let json = Yojson.Safe.from_string body in
  let username = json |> Yojson.Safe.Util.member "username" |> Yojson.Safe.Util.to_string in
  let password = json |> Yojson.Safe.Util.member "password" |> Yojson.Safe.Util.to_string in
  User.authenticate_user t.db ~username ~password >>= function
  | Ok (Some user) ->
      let token = User.generate_token username user.User.role in
      let response = `Assoc [("token", json_of_string token)] in
      Server.respond_string ~status:`OK ~body:(Yojson.Safe.to_string response) ()
  | _ -> Server.respond_string ~status:`Unauthorized ~body:"Invalid credentials" ()

let authenticate_request req =
  match Cohttp.Header.get (Request.headers req) "Authorization" with
  | Some token -> 
      (match User.validate_token token with
       | Some (username, role) -> Some (username, role)
       | None -> None)
  | None -> None

let handle_shorten t req body =
  match authenticate_request req with
  | Some (username, _) ->
      body |> Cohttp_lwt.Body.to_string >>= fun body ->
      let original_url = Uri.of_string body |> Uri.to_string in
      let short_url = shorten_url t original_url username in
      Server.respond_string ~status:`OK ~body:short_url ()
  | None ->
      Server.respond_string ~status:`Unauthorized ~body:"Unauthorized" ()

let handle_request t _conn req body =
  match (Request.meth req, Uri.path (Request.uri req)) with
  | `GET, "/" ->
      Server.respond_string ~status:`OK ~body:"URL Shortener" ()

  | `POST, "/register" ->
      handle_register t body

  | `POST, "/login" ->
      handle_login t body

  | `POST, "/shorten" ->
      handle_shorten t req body

  | `GET, path ->
      let short_code = String.sub path 1 (String.length path - 1) in
      (match get_original_url t short_code with
       | Some original_url ->
           Server.respond_redirect ~uri:(Uri.of_string original_url) ()
       | None ->
           Server.respond_string ~status:`Not_found ~body:"Short URL not found" ())

  | _ ->
      Server.respond_string ~status:`Not_found ~body:"Not found" ()

let start_server t port =
  let callback = handle_request t in
  let server = Server.create ~mode:(`TCP (`Port port)) (Server.make ~callback ()) in
  Lwt_main.run server

let () =
  Random.self_init ();
  User.initialize_db () >>= fun () ->
  let base_url = "http://localhost:8080" in
  create base_url >>= fun t ->
  start_server t 8080