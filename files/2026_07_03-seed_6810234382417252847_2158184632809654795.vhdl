-- Seed: 6810234382417252847,2158184632809654795

use std.reflection.all;

entity fv is
  port (bvpqbg : inout integer_vector(0 to 0); oaa : inout subtype_mirror; rfnrqdqpe : inout file_value_mirror; g : out time);
end fv;

architecture voswc of fv is
  
begin
  -- Single-driven assignments
  g <= g;
  bvpqbg <= bvpqbg;
end voswc;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity nqygnsx is
  port (swp : in std_logic; shdczrqkn : inout value_mirror);
end nqygnsx;

architecture nxokqayos of nqygnsx is
  
begin
  
end nxokqayos;

use std.reflection.all;

entity qebtyzvysa is
  port (bb : inout physical_value_mirror);
end qebtyzvysa;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture sk of qebtyzvysa is
  shared variable tsinu : value_mirror;
  signal cxytv : std_logic;
  signal czxttw : time;
  shared variable zcg : file_value_mirror;
  shared variable vqxrqmpxsl : subtype_mirror;
  signal l : integer_vector(0 to 0);
begin
  dazqu : entity work.fv
    port map (bvpqbg => l, oaa => vqxrqmpxsl, rfnrqdqpe => zcg, g => czxttw);
  i : entity work.nqygnsx
    port map (swp => cxytv, shdczrqkn => tsinu);
  
  -- Multi-driven assignments
  cxytv <= '0';
end sk;



-- Seed after: 14836301696019793463,2158184632809654795
