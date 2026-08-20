-- Seed: 11607042533508258600,499459191852795575

library ieee;
use ieee.std_logic_1164.all;

entity ekmqc is
  port (c : buffer std_logic; eax : out std_logic; ptnoe : inout real);
end ekmqc;

architecture eepfb of ekmqc is
  
begin
  -- Single-driven assignments
  ptnoe <= 42.2_3_1_1;
  
  -- Multi-driven assignments
  eax <= 'X';
  c <= eax;
  eax <= '0';
end eepfb;



-- Seed after: 14069554829539300436,499459191852795575
