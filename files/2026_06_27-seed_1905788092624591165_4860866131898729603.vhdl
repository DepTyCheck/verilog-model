-- Seed: 1905788092624591165,4860866131898729603

library ieee;
use ieee.std_logic_1164.all;

entity u is
  port (d : buffer std_logic_vector(4 to 3));
end u;

architecture cie of u is
  
begin
  -- Multi-driven assignments
  d <= "";
  d <= (others => '0');
  d <= (others => '0');
end cie;



-- Seed after: 2965919269395935218,4860866131898729603
