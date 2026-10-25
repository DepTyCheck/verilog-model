-- Seed: 1828625964198876236,13788294582928374589

library ieee;
use ieee.std_logic_1164.all;

entity hdft is
  port (a : inout std_logic; e : buffer integer);
end hdft;

architecture fuelrhi of hdft is
  
begin
  -- Single-driven assignments
  e <= 2_2_0_0;
  
  -- Multi-driven assignments
  a <= a;
  a <= a;
  a <= '-';
  a <= a;
end fuelrhi;



-- Seed after: 5034516272292075635,13788294582928374589
