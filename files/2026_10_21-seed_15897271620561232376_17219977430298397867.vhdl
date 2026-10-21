-- Seed: 15897271620561232376,17219977430298397867

library ieee;
use ieee.std_logic_1164.all;

entity nrfhch is
  port (zls : buffer std_logic_vector(3 to 3); k : buffer std_logic_vector(2 downto 3));
end nrfhch;

architecture g of nrfhch is
  
begin
  -- Multi-driven assignments
  zls <= zls;
  k <= (others => '0');
end g;



-- Seed after: 4800792937777546330,17219977430298397867
