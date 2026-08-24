-- Seed: 17134364022356889933,16159265764638711791

library ieee;
use ieee.std_logic_1164.all;

entity tpt is
  port (abm : buffer std_logic_vector(1 downto 2));
end tpt;

architecture lubo of tpt is
  
begin
  -- Multi-driven assignments
  abm <= (others => '0');
end lubo;



-- Seed after: 409609196933622201,16159265764638711791
