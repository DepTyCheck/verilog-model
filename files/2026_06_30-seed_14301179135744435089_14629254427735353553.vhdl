-- Seed: 14301179135744435089,14629254427735353553

library ieee;
use ieee.std_logic_1164.all;

entity gxo is
  port (ocyphu : buffer std_logic; byu : inout time);
end gxo;

architecture xh of gxo is
  
begin
  -- Single-driven assignments
  byu <= 1431 ns;
  
  -- Multi-driven assignments
  ocyphu <= '0';
  ocyphu <= 'U';
  ocyphu <= '-';
end xh;



-- Seed after: 12831680958755538329,14629254427735353553
