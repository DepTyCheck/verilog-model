-- Seed: 3028323138098195088,14629254427735353553

library ieee;
use ieee.std_logic_1164.all;

entity dtcscofo is
  port (ozjbwq : inout boolean_vector(2 to 2); qunrep : out std_logic_vector(3 downto 1); k : inout time);
end dtcscofo;

architecture bsg of dtcscofo is
  
begin
  -- Single-driven assignments
  ozjbwq <= (others => TRUE);
  
  -- Multi-driven assignments
  qunrep <= ('Z', 'L', 'U');
  qunrep <= "-UH";
end bsg;



-- Seed after: 14799729912544037959,14629254427735353553
