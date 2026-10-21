-- Seed: 2597776383950312088,17219977430298397867

library ieee;
use ieee.std_logic_1164.all;

entity xnbbup is
  port (f : out time; v : in std_logic; aoka : buffer time);
end xnbbup;

architecture s of xnbbup is
  
begin
  -- Single-driven assignments
  aoka <= 1402.0_4_0 ps;
  f <= 2_1.0_2_2 ps;
end s;



-- Seed after: 8399396938898777780,17219977430298397867
