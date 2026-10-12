-- Seed: 6099082040463261700,6869379746012019351

library ieee;
use ieee.std_logic_1164.all;

entity maw is
  port (mhnu : in std_logic_vector(3 downto 2); k : inout integer);
end maw;

architecture umai of maw is
  
begin
  -- Single-driven assignments
  k <= 8#57#;
end umai;



-- Seed after: 15345717719621709964,6869379746012019351
