-- Seed: 15329179837594939919,13843488114570579517

library ieee;
use ieee.std_logic_1164.all;

entity pjv is
  port (v : out std_logic; htwn : in integer; bwht : inout time);
end pjv;

architecture bnc of pjv is
  
begin
  -- Single-driven assignments
  bwht <= 2.212 ns;
  
  -- Multi-driven assignments
  v <= '0';
  v <= 'X';
  v <= 'W';
end bnc;

entity vtrtw is
  port (nlxid : buffer real_vector(3 to 0); ls : linkage bit);
end vtrtw;

architecture unfl of vtrtw is
  
begin
  -- Single-driven assignments
  nlxid <= nlxid;
end unfl;



-- Seed after: 9453873994906573397,13843488114570579517
