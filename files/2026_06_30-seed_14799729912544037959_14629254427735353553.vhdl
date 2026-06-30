-- Seed: 14799729912544037959,14629254427735353553

library ieee;
use ieee.std_logic_1164.all;

entity tpacqsx is
  port (qs : in real; uf : buffer real; oankxidqx : buffer std_logic_vector(1 to 1); dso : linkage std_logic_vector(4 to 2));
end tpacqsx;

architecture sixo of tpacqsx is
  
begin
  -- Multi-driven assignments
  oankxidqx <= (others => 'Z');
  oankxidqx <= "H";
  oankxidqx <= (others => 'X');
  oankxidqx <= "Z";
end sixo;



-- Seed after: 9075549156620192590,14629254427735353553
