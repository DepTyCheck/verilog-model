-- Seed: 14328124013161848198,17047277710231705797

library ieee;
use ieee.std_logic_1164.all;

entity tkiuw is
  port (uuctq : out std_logic_vector(3 downto 0); qtlsw : out time; sujask : in std_logic_vector(3 to 1));
end tkiuw;

architecture u of tkiuw is
  
begin
  -- Single-driven assignments
  qtlsw <= 3_1_1_1_0.3_2_2 us;
  
  -- Multi-driven assignments
  uuctq <= ('-', 'H', 'L', 'H');
  uuctq <= ('H', 'X', 'W', 'X');
  uuctq <= ('Z', 'L', 'W', '0');
  uuctq <= ('0', 'Z', 'X', 'H');
end u;



-- Seed after: 9660605018414929008,17047277710231705797
