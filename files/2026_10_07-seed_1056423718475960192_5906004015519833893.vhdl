-- Seed: 1056423718475960192,5906004015519833893

library ieee;
use ieee.std_logic_1164.all;

entity tawr is
  port (kx : buffer real_vector(1 downto 4); piyqu : out std_logic_vector(4 downto 1));
end tawr;

architecture rbbtf of tawr is
  
begin
  -- Single-driven assignments
  kx <= (others => 0.0);
  
  -- Multi-driven assignments
  piyqu <= ('1', 'X', '0', '0');
  piyqu <= ('L', 'H', 'Z', 'X');
  piyqu <= piyqu;
  piyqu <= piyqu;
end rbbtf;



-- Seed after: 13271260810926247665,5906004015519833893
