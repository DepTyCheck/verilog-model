-- Seed: 9399369490985632782,7198033922882419595

library ieee;
use ieee.std_logic_1164.all;

entity au is
  port (irbke : inout std_logic_vector(2 downto 3); m : buffer integer);
end au;

architecture ceaavm of au is
  
begin
  -- Single-driven assignments
  m <= 0;
  
  -- Multi-driven assignments
  irbke <= irbke;
  irbke <= irbke;
end ceaavm;



-- Seed after: 18271155752326305069,7198033922882419595
