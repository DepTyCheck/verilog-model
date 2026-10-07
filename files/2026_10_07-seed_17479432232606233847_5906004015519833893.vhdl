-- Seed: 17479432232606233847,5906004015519833893

library ieee;
use ieee.std_logic_1164.all;

entity crtawzyb is
  port (dalk : inout std_logic_vector(0 to 0));
end crtawzyb;

architecture om of crtawzyb is
  
begin
  -- Multi-driven assignments
  dalk <= (others => '0');
  dalk <= dalk;
  dalk <= (others => 'X');
  dalk <= dalk;
end om;



-- Seed after: 2894914100726143816,5906004015519833893
