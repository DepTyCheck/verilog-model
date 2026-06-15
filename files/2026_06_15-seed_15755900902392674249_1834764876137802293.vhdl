-- Seed: 15755900902392674249,1834764876137802293

library ieee;
use ieee.std_logic_1164.all;

entity obugb is
  port (ymam : inout std_logic_vector(1 downto 1));
end obugb;

architecture tp of obugb is
  
begin
  -- Multi-driven assignments
  ymam <= "L";
  ymam <= (others => 'H');
  ymam <= "-";
end tp;



-- Seed after: 1756438177549189210,1834764876137802293
