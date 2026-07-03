-- Seed: 2094578830528058962,2158184632809654795

entity bvnwhap is
  port (hwuvym : out bit_vector(3 to 2));
end bvnwhap;

architecture x of bvnwhap is
  
begin
  -- Single-driven assignments
  hwuvym <= (others => '0');
end x;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity hv is
  port (erhdr : inout subtype_mirror; rawosl : linkage real_vector(3 to 0); kekjmr : out std_logic; u : in time);
end hv;

architecture qo of hv is
  signal tzhvrxbgtp : bit_vector(3 to 2);
  signal zgtros : bit_vector(3 to 2);
  signal mbv : bit_vector(3 to 2);
begin
  b : entity work.bvnwhap
    port map (hwuvym => mbv);
  xkkea : entity work.bvnwhap
    port map (hwuvym => zgtros);
  iiy : entity work.bvnwhap
    port map (hwuvym => tzhvrxbgtp);
  
  -- Multi-driven assignments
  kekjmr <= 'W';
  kekjmr <= '0';
end qo;



-- Seed after: 11210445999825228423,2158184632809654795
