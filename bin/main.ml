module F = Format

open Raycaml

let sample_image() = 
  let image_width = 256 in
  let image_height = 256 in
  let render ~row ~col = 
    let r = float_of_int row /. (float_of_int (image_height -1)) in
    let g = float_of_int col /. (float_of_int (image_width -1)) in
    let b= 0.1 in 
    let ir = 255.999 *. r |> int_of_float in
    let ig = 255.999 *. g |> int_of_float in
    let ib = 255.999 *. b |> int_of_float in
    Pixel.create (ir, ig, ib)
  in
  Image.create ~height:image_height ~width:image_width ~f:render

let raytraced_image () =
  let aspect_ratio = 16.0 /. 9.0 in 
  let image_width = 400 in
  (* calculate the image height *)
  let image_height = float_of_int image_width /. aspect_ratio |> int_of_float in
  let viewport_height = 2.0 in
  let viewport_width =
    viewport_height *. (float_of_int image_width /. float_of_int image_height)
  in
  let camera_center = Point3d.(0., 0., 0.) in 
  
  (* create the image *)
  Image.create ~height:image_height ~width:image_width ~f:(fun ~row ~col -> Pixel.create (row, col, 0))

let () = 
  let oc = Out_channel.open_text "sample.ppm" in
  try 
    let fmt = F.formatter_of_out_channel oc in
    let img = sample_image () in
    F.fprintf fmt "%a" Image.pp_ppm img;
    Out_channel.close oc;
    F.printf "Successfuly generated an image @\n";
with ex ->
  F.eprintf "%s" (Printexc.to_string ex);
  Out_channel.close_noerr oc
 