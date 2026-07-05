-- Seed: 10569458686265038949,3181554006726329157

use std.reflection.all;

entity jum is
  port (pts : inout array_value_mirror);
end jum;

architecture vh of jum is
  
begin
  
end vh;

use std.reflection.all;

entity ty is
  port (budrru : inout access_value_mirror; hxglsm : inout record_value_mirror);
end ty;

architecture qla of ty is
  
begin
  
end qla;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity ktnelrby is
  port (fjmtebzrnm : inout std_logic_vector(3 downto 1); nfjts : buffer time; ryvx : inout array_subtype_mirror);
end ktnelrby;

use std.reflection.all;

architecture tkooejwr of ktnelrby is
  shared variable kleqgiz : record_value_mirror;
  shared variable hw : access_value_mirror;
  shared variable ibr : record_value_mirror;
  shared variable keohfpgdx : access_value_mirror;
begin
  ncsj : entity work.ty
    port map (budrru => keohfpgdx, hxglsm => ibr);
  igikf : entity work.ty
    port map (budrru => hw, hxglsm => kleqgiz);
  
  -- Single-driven assignments
  nfjts <= nfjts;
end tkooejwr;

entity lyjbttxak is
  port (i : buffer time);
end lyjbttxak;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture xrmikbq of lyjbttxak is
  shared variable g : array_subtype_mirror;
  signal ubuvonx : time;
  signal rfnn : std_logic_vector(3 downto 1);
begin
  rtmry : entity work.ktnelrby
    port map (fjmtebzrnm => rfnn, nfjts => ubuvonx, ryvx => g);
  
  -- Single-driven assignments
  i <= i;
end xrmikbq;



-- Seed after: 12168152700121519295,3181554006726329157
