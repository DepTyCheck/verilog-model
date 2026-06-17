-- Seed: 3794690592800386776,10557070023141912087

library ieee;
use ieee.std_logic_1164.all;

entity ci is
  port (k : inout std_logic_vector(1 downto 1));
end ci;

architecture itlb of ci is
  
begin
  -- Multi-driven assignments
  k <= (others => 'H');
  k <= "Z";
end itlb;



-- Seed after: 10578459461261078398,10557070023141912087
