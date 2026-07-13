-- Seed: 6566289968181739320,3566912872917928779

use std.reflection.all;

entity dny is
  port (rap : inout integer_value_mirror);
end dny;

architecture v of dny is
  
begin
  
end v;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity h is
  port (tm : inout std_logic_vector(2 to 3); hojenbi : inout subtype_mirror; ippsiz : in integer);
end h;

use std.reflection.all;

architecture oychqd of h is
  shared variable ypp : integer_value_mirror;
  shared variable mwlgshzwv : integer_value_mirror;
  shared variable ippnyp : integer_value_mirror;
begin
  eckudki : entity work.dny
    port map (rap => ippnyp);
  gdfb : entity work.dny
    port map (rap => mwlgshzwv);
  i : entity work.dny
    port map (rap => ypp);
  
  -- Multi-driven assignments
  tm <= tm;
  tm <= "U1";
end oychqd;

use std.reflection.all;

entity p is
  port (cguxfgzry : inout access_subtype_mirror; u : in integer);
end p;

use std.reflection.all;

architecture vopl of p is
  shared variable dxkrbcnqg : integer_value_mirror;
  shared variable svlcgvftpe : integer_value_mirror;
begin
  ons : entity work.dny
    port map (rap => svlcgvftpe);
  aqpyvdcg : entity work.dny
    port map (rap => dxkrbcnqg);
end vopl;



-- Seed after: 3415903789359849425,3566912872917928779
