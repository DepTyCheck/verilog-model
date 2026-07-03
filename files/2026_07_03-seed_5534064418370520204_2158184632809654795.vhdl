-- Seed: 5534064418370520204,2158184632809654795

use std.reflection.all;

entity j is
  port (hgkpj : inout record_value_mirror);
end j;

architecture utyvwj of j is
  
begin
  
end utyvwj;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity zowfdrfth is
  port (lae : inout std_logic_vector(1 to 1); knfgek : buffer integer; zydk : inout integer_value_mirror);
end zowfdrfth;

use std.reflection.all;

architecture ul of zowfdrfth is
  shared variable pv : record_value_mirror;
  shared variable oxljgj : record_value_mirror;
begin
  dqtnxhxx : entity work.j
    port map (hgkpj => oxljgj);
  u : entity work.j
    port map (hgkpj => pv);
  
  -- Single-driven assignments
  knfgek <= 0_2_0;
  
  -- Multi-driven assignments
  lae <= lae;
  lae <= lae;
  lae <= lae;
end ul;



-- Seed after: 6495753245301434498,2158184632809654795
