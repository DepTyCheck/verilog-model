-- Seed: 15403629519382300536,1112937151005418631

library ieee;
use ieee.std_logic_1164.all;

entity uu is
  port (rvdyhdnl : buffer std_logic_vector(0 to 1); jiyp : inout std_logic; y : buffer bit);
end uu;

architecture cls of uu is
  
begin
  -- Single-driven assignments
  y <= '1';
  
  -- Multi-driven assignments
  jiyp <= '-';
  rvdyhdnl <= ('L', 'Z');
end cls;



-- Seed after: 11353139578938778488,1112937151005418631
