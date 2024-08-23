open Lwt.Infix
open Lwt_result.Infix

let get_db_connection () =
  let uri = Uri.of_string "sqlite3:url_shortener.db" in
  Caqti_lwt.connect uri

let db = 
  match Lwt_main.run (get_db_connection ()) with
  | Ok db -> db
  | Error err -> failwith (Caqti_error.show err)

type role = Admin | Normal | VIP

type t = {
  id: int64;
  username: string;
  password_hash: string;
  role: role;
}

let role_of_string = function
  | "admin" -> Admin
  | "vip" -> VIP
  | _ -> Normal

let string_of_role = function
  | Admin -> "admin"
  | VIP -> "vip"
  | Normal -> "normal"

let user_type =
  let encode (id, username, password_hash, role) =
    Ok (id, username, password_hash, string_of_role role)
  in
  let decode (id, username, password_hash, role_str) =
    Ok (id, username, password_hash, role_of_string role_str)
  in
  Caqti_type.(custom ~encode ~decode (t4 int64 string string string))

let create_table =
  get_db_connection () >>= function
  | Ok db -> 
      Caqti_lwt.exec db Caqti_type.unit
        {|
        CREATE TABLE IF NOT EXISTS users (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          username TEXT NOT NULL UNIQUE,
          password_hash TEXT NOT NULL,
          role TEXT NOT NULL DEFAULT 'normal'
        )
        |} ()
  | Error err -> Lwt.fail (Failure (Caqti_error.show err))

let insert_user =
  Caqti_lwt.exec Caqti_type.(t3 string string string)
    "INSERT INTO users (username, password_hash, role) VALUES (?, ?, ?)"

let find_user_by_username =
  Caqti_lwt.find_opt user_type
    "SELECT id, username, password_hash, role FROM users WHERE username = ?"

let update_user_role =
  Caqti_lwt.exec Caqti_type.(t2 string int64)
    "UPDATE users SET role = ? WHERE id = ?"

let hash_password password =
  Argon2.hash ~t_cost:2 ~m_cost:(1 lsl 16) ~parallelism:2 password

let verify_password password hash =
  Argon2.verify ~hash password

let create_user dbh ~username ~password =
  hash_password password >>= fun password_hash ->
  insert_user dbh (username, password_hash, "normal")

let authenticate_user dbh ~username ~password =
  find_user_by_username dbh username >>= function
  | Some (id, _, password_hash, role) ->
      if verify_password password password_hash then
        Lwt.return_ok (Some {id; username; password_hash; role})
      else
        Lwt.return_ok None
  | None -> Lwt.return_ok None

let secret_key = "your_secret_key_here" (* In production, use a secure, environment-specific secret *)

let generate_token username role =
  let payload = `Assoc [
    ("username", `String username);
    ("role", `String (string_of_role role));
    ("exp", `Int (int_of_float (Unix.time () +. 86400.0))) (* 24 hour expiration *)
  ] in
  let header = Jose.Header.make_header Jose.JWS.HS256 in
  let jwt = Jose.Jwt.sign ~header ~payload secret_key in
  Jose.Jwt.to_string jwt

let validate_token token =
  try
    let jwt = Jose.Jwt.of_string token in
    match Jose.Jwt.validate ~secret:secret_key jwt with
    | Ok payload ->
        let username = payload |> Jose.Jwt.payload_of |> Yojson.Safe.Util.member "username" |> Yojson.Safe.Util.to_string in
        let role = payload |> Jose.Jwt.payload_of |> Yojson.Safe.Util.member "role" |> Yojson.Safe.Util.to_string |> role_of_string in
        Some (username, role)
    | Error _ -> None
  with _ -> None

let get_db_connection () =
  Db.connect (Uri.of_string "sqlite3:url_shortener.db")

let initialize_db () =
  get_db_connection () >>= function
  | Ok dbh ->
      create_table dbh >>= function
      | Ok () -> Lwt.return_unit
      | Error err -> Lwt_io.printf "Error creating table: %s\n" (Caqti_error.show err)
  | Error err -> Lwt_io.printf "Error connecting to database: %s\n" (Caqti_error.show err)