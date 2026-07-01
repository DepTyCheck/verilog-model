-- Seed: 17944300805294836825,6882842853887419669

library ieee;
use ieee.std_logic_1164.all;

entity fs is
  port (fcz : inout std_logic_vector(0 to 2); sam : inout bit_vector(3 downto 4); kopdhnn : out std_logic);
end fs;

architecture b of fs is
  
begin
  -- Single-driven assignments
  sam <= (others => '0');
  
  -- Multi-driven assignments
  kopdhnn <= 'U';
  kopdhnn <= '0';
  kopdhnn <= 'W';
end b;



-- Seed after: 6385929491276913719,6882842853887419669
