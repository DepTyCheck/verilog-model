-- Seed: 4937521540587061767,6377581586043967497

library ieee;
use ieee.std_logic_1164.all;

entity tsdsqggy is
  port (rsmd : out integer; fbk : out integer; bdx : buffer std_logic_vector(4 downto 3));
end tsdsqggy;

architecture cymlw of tsdsqggy is
  
begin
  -- Single-driven assignments
  rsmd <= fbk;
  
  -- Multi-driven assignments
  bdx <= ('L', 'U');
  bdx <= bdx;
  bdx <= bdx;
end cymlw;



-- Seed after: 10921669031807313631,6377581586043967497
