-- Seed: 6155378295180398712,7808623373429384027

library ieee;
use ieee.std_logic_1164.all;

entity ii is
  port (k : out std_logic_vector(0 to 1));
end ii;

architecture e of ii is
  
begin
  -- Multi-driven assignments
  k <= ('Z', 'W');
  k <= "WU";
  k <= k;
  k <= k;
end e;



-- Seed after: 16006609900032400435,7808623373429384027
