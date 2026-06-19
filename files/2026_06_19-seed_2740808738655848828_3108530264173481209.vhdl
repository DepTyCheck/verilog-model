-- Seed: 2740808738655848828,3108530264173481209

library ieee;
use ieee.std_logic_1164.all;

entity ywbgute is
  port (umrn : linkage integer; ob : out std_logic_vector(2 downto 0); hy : out integer);
end ywbgute;

architecture ig of ywbgute is
  
begin
  -- Single-driven assignments
  hy <= 02;
  
  -- Multi-driven assignments
  ob <= ('-', 'U', 'X');
end ig;



-- Seed after: 601697517563804056,3108530264173481209
