-- Seed: 3874194653301426952,6176025545472508329

library ieee;
use ieee.std_logic_1164.all;

entity k is
  port (h : inout std_logic);
end k;

architecture y of k is
  
begin
  -- Multi-driven assignments
  h <= 'W';
  h <= 'X';
  h <= h;
  h <= h;
end y;



-- Seed after: 1856628246602496278,6176025545472508329
