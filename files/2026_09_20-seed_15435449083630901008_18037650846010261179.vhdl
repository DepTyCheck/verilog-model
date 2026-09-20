-- Seed: 15435449083630901008,18037650846010261179

library ieee;
use ieee.std_logic_1164.all;

entity d is
  port (drq : inout std_logic_vector(1 to 1));
end d;

architecture u of d is
  
begin
  -- Multi-driven assignments
  drq <= "0";
  drq <= (others => 'U');
  drq <= "-";
end u;



-- Seed after: 12650838016175842875,18037650846010261179
