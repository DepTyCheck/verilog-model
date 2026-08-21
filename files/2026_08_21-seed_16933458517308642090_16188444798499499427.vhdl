-- Seed: 16933458517308642090,16188444798499499427

library ieee;
use ieee.std_logic_1164.all;

entity pnuavkpex is
  port (b : out integer_vector(3 downto 0); fpxbu : inout std_logic; c : buffer integer);
end pnuavkpex;

architecture sqcfgh of pnuavkpex is
  
begin
  -- Single-driven assignments
  c <= 02;
  b <= (8#4_0_2#, 8#7#, 3_2_2_3_2, 1024);
  
  -- Multi-driven assignments
  fpxbu <= fpxbu;
  fpxbu <= fpxbu;
  fpxbu <= 'W';
end sqcfgh;



-- Seed after: 12583909105048709613,16188444798499499427
