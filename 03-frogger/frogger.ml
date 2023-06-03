open Base
open Scaffold

(* When you start the exercise, the compiler will complain that Frog.create,
 * World.create and create_frog are unused. You can remove this attribute once
 * you get going. *)
[@@@warning "-32"]

module Orientation = struct
  type t =
    | Up
    | Right
    | Down
    | Left
end

module Frog = struct
  type t =
    { position : Position.t
    ; orientation : Orientation.t
    } [@@deriving fields]

  let create = Fields.create
end

module World = struct
  type t =
    { frog  : Frog.t
    } [@@deriving fields]

  let create = Fields.create
end

let create_frog () =
  let x = Random.int 1 in
  let y = Random.int 1 in
  let position = Position.create ~x:x ~y:y in
  Frog.create ~position:position ~orientation:Up
;;

let create () =
  let frog = create_frog () in
  World.create ~frog:frog
;;

let tick (world : World.t) = world
  (* failwith *)
  (*   "This function will end up getting called every timestep, which happens to \ *)
  (*    be set to 1 second for this game in the scaffold (so you can easily see \ *)
  (*    what's going on). For the first step (just moving the frog/camel around), \ *)
  (*    you can just return [world] here. Later you'll want do interesting things \ *)
  (*    like move all the cars and logs, detect collisions and figure out if the \ *)
  (*    player has died or won. " *)
;;

let handle_input (world : World.t) (key : Key.t) =
  let frog_position = world.frog.position in
  let frog_orientation = world.frog.orientation in
  let delta_x, delta_y, orientation = match key with
  | Key.Arrow_down -> if frog_position.y >= 0 then (0, -1, Orientation.Down) else (0, 0, frog_orientation)
  | Key.Arrow_up -> if frog_position.y < 10 then (0, 1, Orientation.Up) else (0, 0, frog_orientation)
  | Key.Arrow_right -> if frog_position.x <= 10 then (1, 0, Orientation.Right) else (0, 0, frog_orientation)
  | Key.Arrow_left -> if frog_position.x > 0 then (-1, 0, Orientation.Left) else (0, 0, frog_orientation)
  in
  let new_frog_position = Position.create ~x:(frog_position.x + delta_x) ~y:(frog_position.y + delta_y) in
  let new_frog = Frog.create ~position:new_frog_position ~orientation in
  World.create ~frog:new_frog
  (*   This function will end up getting called whenever the player presses one of \ *)
  (*    the four arrow keys. What should the new state of the world be? Create and \ *)
  (*    return it based on the current state of the world (the [world] argument), \ *)
  (*    and the key that was pressed ([key]). Use either [World.create] or the \ *)
  (*    record update syntax: *)
  (*   { world with frog = Frog.create ... } *)
;;

let draw (world : World.t) =
  let frog_position = world.frog.position in
  let frog_orientation = world.frog.orientation in
  let image = match frog_orientation with
    | Up -> Image.Frog_up
    | Down -> Image.Frog_down
    | Right -> Image.Frog_right
    | Left -> Image.Frog_left
  in
  [(image, frog_position)]
  (* failwith *)
  (*   "Return a list with a single item: a tuple consisting of one of the choices \ *)
  (*    in [Images.t] in [scaffold.mli]; and the current position of the [Frog]." *)
;;

let handle_event (world : World.t) (event : Event.t) =
  match event with
  | Event.Keypress key -> handle_input world key
  | Event.Tick -> tick world
  (* failwith *)
  (*   "This function should probably be just 3 lines long: [match event with ...]" *)
;;

let finished (_ : World.t) = false
  (* failwith *)
  (*   "This can probably just return [false] in the beginning." *)
;;
