-- Seed: 14323805674362323213,13843488114570579517

library ieee;
use ieee.std_logic_1164.all;

entity d is
  port (lv : inout real; kn : out bit; mwma : linkage std_logic_vector(4 downto 2));
end d;

architecture tso of d is
  
begin
  -- Single-driven assignments
  kn <= '0';
  lv <= 2#111.1_0_0_0_0#;
end tso;



-- Seed after: 74787538619899918,13843488114570579517
