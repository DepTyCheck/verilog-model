-- Seed: 4150873000716357598,3566912872917928779

entity bhnveltz is
  port (lgfbwldy : buffer real);
end bhnveltz;

architecture gbwnlujo of bhnveltz is
  
begin
  -- Single-driven assignments
  lgfbwldy <= 16#B_D.3D#;
end gbwnlujo;

use std.reflection.all;

entity ewzonnuuap is
  port (uqfkgetl : inout record_value_mirror);
end ewzonnuuap;

architecture ypazihv of ewzonnuuap is
  signal vu : real;
begin
  zbibjbfwi : entity work.bhnveltz
    port map (lgfbwldy => vu);
end ypazihv;

use std.reflection.all;

entity jrvggm is
  port (ysehlbpb : in boolean; ucike : inout integer; r : inout access_value_mirror; rh : inout enumeration_value_mirror);
end jrvggm;

use std.reflection.all;

architecture oxvq of jrvggm is
  shared variable j : record_value_mirror;
begin
  a : entity work.ewzonnuuap
    port map (uqfkgetl => j);
  
  -- Single-driven assignments
  ucike <= 8#1#;
end oxvq;

use std.reflection.all;

entity nphexm is
  port (b : inout record_subtype_mirror; zxmdxjy : inout file_value_mirror; hfntbowi : inout integer_subtype_mirror);
end nphexm;

use std.reflection.all;

architecture ontiz of nphexm is
  signal jxe : real;
  shared variable hmzhctd : enumeration_value_mirror;
  shared variable qe : access_value_mirror;
  signal amne : integer;
  signal o : boolean;
begin
  x : entity work.jrvggm
    port map (ysehlbpb => o, ucike => amne, r => qe, rh => hmzhctd);
  dlvbdsxl : entity work.bhnveltz
    port map (lgfbwldy => jxe);
  
  -- Single-driven assignments
  o <= o;
end ontiz;



-- Seed after: 4393711622988013555,3566912872917928779
