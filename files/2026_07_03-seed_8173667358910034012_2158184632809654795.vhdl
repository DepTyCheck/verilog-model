-- Seed: 8173667358910034012,2158184632809654795

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity izwkhde is
  port (znatzxq : inout integer; malaklhgu : inout value_mirror; ecdnkx : in std_logic_vector(2 to 3));
end izwkhde;

architecture pd of izwkhde is
  
begin
  -- Single-driven assignments
  znatzxq <= znatzxq;
end pd;

entity s is
  port (ihjjwtvk : in integer);
end s;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture xbln of s is
  signal yjthn : std_logic_vector(2 to 3);
  shared variable q : value_mirror;
  signal cuhfxktxot : integer;
begin
  hikauowqkp : entity work.izwkhde
    port map (znatzxq => cuhfxktxot, malaklhgu => q, ecdnkx => yjthn);
  
  -- Multi-driven assignments
  yjthn <= ('H', '1');
end xbln;

use std.reflection.all;

entity zjpflxx is
  port (lm : inout enumeration_subtype_mirror; esbkyh : out time);
end zjpflxx;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture o of zjpflxx is
  signal rhu : std_logic_vector(2 to 3);
  shared variable nb : value_mirror;
  signal n : std_logic_vector(2 to 3);
  shared variable zcvwzluoj : value_mirror;
  signal isiyypy : integer;
  signal d : integer;
begin
  jgvlms : entity work.s
    port map (ihjjwtvk => d);
  zg : entity work.izwkhde
    port map (znatzxq => isiyypy, malaklhgu => zcvwzluoj, ecdnkx => n);
  uzjdkxxhkx : entity work.izwkhde
    port map (znatzxq => d, malaklhgu => nb, ecdnkx => rhu);
  
  -- Multi-driven assignments
  n <= "0X";
  rhu <= ('H', '1');
  n <= n;
  rhu <= "Z-";
end o;



-- Seed after: 14418173394973019395,2158184632809654795
