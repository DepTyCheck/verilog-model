-- Seed: 9983036068707067596,3566912872917928779

use std.reflection.all;

entity eeodw is
  port (zx : in real; zzvtvgth : inout array_value_mirror);
end eeodw;

architecture ixkfpqcrka of eeodw is
  
begin
  
end ixkfpqcrka;

entity hd is
  port (hurfmdf : in bit);
end hd;

use std.reflection.all;

architecture hakwjqofls of hd is
  shared variable p : array_value_mirror;
  signal m : real;
  shared variable zzv : array_value_mirror;
  signal frfkhh : real;
begin
  ff : entity work.eeodw
    port map (zx => frfkhh, zzvtvgth => zzv);
  pghjriiyx : entity work.eeodw
    port map (zx => m, zzvtvgth => p);
  
  -- Single-driven assignments
  frfkhh <= frfkhh;
  m <= frfkhh;
end hakwjqofls;



-- Seed after: 9529494543256784195,3566912872917928779
