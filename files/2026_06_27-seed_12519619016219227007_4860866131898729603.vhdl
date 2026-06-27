-- Seed: 12519619016219227007,4860866131898729603

library ieee;
use ieee.std_logic_1164.all;

entity bc is
  port (ytti : inout real; xwori : inout bit_vector(3 downto 3); axj : out std_logic_vector(4 downto 2));
end bc;

architecture uprxamfnt of bc is
  
begin
  -- Single-driven assignments
  ytti <= 00.1;
  xwori <= (others => '1');
  
  -- Multi-driven assignments
  axj <= ('W', '-', '-');
  axj <= ('-', 'X', 'H');
  axj <= ('L', 'H', '1');
  axj <= "ZUZ";
end uprxamfnt;



-- Seed after: 4219565604604593479,4860866131898729603
