module F = Format

open Raycaml


let%expect_test _ =
  let p = Pixel.create(0, 0, 0) in
  F.printf "%a" Pixel.pp_ppm p;
  [%expect {| 0   0   0 |}];

  let p = Pixel.create(10, 20, 30) in
  F.printf "%a" Pixel.pp_ppm p;
  [%expect {| 10  20  10 |}]


let%expect_test _ =
  let img = Image.create ~height:2 ~width:3 ~f:(fun ~row ~col -> Pixel.create (row, col, 0) ) in
  F.printf "%a" Image.pp_ppm img;
  [%expect {|
    P3
    3 2
    255
      0   0   0
      0   1   0
      0   2   0
      1   0   1
      1   1   1
      1   2   1
    |}]

