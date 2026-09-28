-- Seed: 15619173690356096467,7311216359267151659

library ieee;
use ieee.std_logic_1164.all;

entity urv is
  port (umsp : in integer; t : out std_logic_vector(1 downto 1));
end urv;

architecture zpnb of urv is
  
begin
  -- Multi-driven assignments
  t <= t;
  t <= t;
  t <= (others => 'U');
  t <= t;
end zpnb;



-- Seed after: 11800452978640933349,7311216359267151659
