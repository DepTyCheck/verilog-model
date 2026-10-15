-- Seed: 3401457084974802925,6377581586043967497

library ieee;
use ieee.std_logic_1164.all;

entity j is
  port (sjbmd : inout std_logic_vector(3 downto 4));
end j;

architecture upa of j is
  
begin
  -- Multi-driven assignments
  sjbmd <= sjbmd;
  sjbmd <= "";
end upa;



-- Seed after: 17184868837246487663,6377581586043967497
