-- Seed: 1970114142234004853,14426950258250697445

use std.reflection.all;

entity g is
  port (tubmdpcgxx : inout integer_value_mirror);
end g;

architecture yikt of g is
  
begin
  
end yikt;

use std.reflection.all;

entity ro is
  port (tgzrrtld : inout access_value_mirror);
end ro;

use std.reflection.all;

architecture qlqxrbbnu of ro is
  shared variable pnmyekopj : integer_value_mirror;
begin
  lguf : entity work.g
    port map (tubmdpcgxx => pnmyekopj);
end qlqxrbbnu;

use std.reflection.all;

entity ctlrgq is
  port (ywwttob : out integer; iqbzvlb : inout subtype_mirror; irrmre : linkage time);
end ctlrgq;

use std.reflection.all;

architecture balgbvl of ctlrgq is
  shared variable hm : integer_value_mirror;
  shared variable eng : access_value_mirror;
  shared variable koqwev : access_value_mirror;
  shared variable saaam : integer_value_mirror;
begin
  ceggzivqcm : entity work.g
    port map (tubmdpcgxx => saaam);
  hrolq : entity work.ro
    port map (tgzrrtld => koqwev);
  xixzvpm : entity work.ro
    port map (tgzrrtld => eng);
  p : entity work.g
    port map (tubmdpcgxx => hm);
  
  -- Single-driven assignments
  ywwttob <= ywwttob;
end balgbvl;



-- Seed after: 17738006585933358965,14426950258250697445
