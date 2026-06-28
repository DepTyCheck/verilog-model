-- Seed: 122670723179059196,6697892553037813751

library ieee;
use ieee.std_logic_1164.all;

entity cr is
  port (z : out std_logic_vector(2 downto 2); zfbclf : out std_logic_vector(1 downto 2));
end cr;

architecture ql of cr is
  
begin
  -- Multi-driven assignments
  z <= (others => '1');
  zfbclf <= (others => '0');
  zfbclf <= "";
  z <= (others => 'L');
end ql;



-- Seed after: 4752048588560751808,6697892553037813751
