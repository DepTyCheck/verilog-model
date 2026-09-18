-- Seed: 17452387250684253324,3316342841050048249

library ieee;
use ieee.std_logic_1164.all;

entity vga is
  port (shtabio : buffer std_logic);
end vga;

architecture q of vga is
  
begin
  -- Multi-driven assignments
  shtabio <= 'U';
  shtabio <= shtabio;
end q;



-- Seed after: 2158077451750007693,3316342841050048249
