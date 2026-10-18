-- Seed: 14843566802903030782,14108379317686958507

library ieee;
use ieee.std_logic_1164.all;

entity tmfa is
  port (wvqmdbgcc : inout time_vector(4 to 1); ajs : out std_logic_vector(4 downto 2); n : buffer std_logic_vector(2 to 0); encji : in integer);
end tmfa;

architecture j of tmfa is
  
begin
  -- Single-driven assignments
  wvqmdbgcc <= (others => 0 ns);
  
  -- Multi-driven assignments
  n <= "";
  n <= n;
end j;



-- Seed after: 1071978351587390279,14108379317686958507
