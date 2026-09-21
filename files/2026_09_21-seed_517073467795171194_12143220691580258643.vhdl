-- Seed: 517073467795171194,12143220691580258643

library ieee;
use ieee.std_logic_1164.all;

entity jb is
  port (eqcx : buffer std_logic; kxpzb : buffer bit; kjlvrqot : out severity_level; jzhnvj : inout std_logic_vector(2 downto 0));
end jb;

architecture emzvvbt of jb is
  
begin
  -- Single-driven assignments
  kjlvrqot <= WARNING;
  kxpzb <= '0';
  
  -- Multi-driven assignments
  eqcx <= '1';
  eqcx <= 'Z';
  eqcx <= eqcx;
end emzvvbt;



-- Seed after: 11004624887558365251,12143220691580258643
