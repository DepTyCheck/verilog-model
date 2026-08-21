-- Seed: 15521887041466926332,16188444798499499427

library ieee;
use ieee.std_logic_1164.all;

entity opbr is
  port (ondqdc : out std_logic_vector(4 downto 4));
end opbr;

architecture atma of opbr is
  
begin
  -- Multi-driven assignments
  ondqdc <= "L";
  ondqdc <= ondqdc;
  ondqdc <= "Z";
end atma;



-- Seed after: 8295688912320283212,16188444798499499427
