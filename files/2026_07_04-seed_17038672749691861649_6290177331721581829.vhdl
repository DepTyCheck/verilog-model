-- Seed: 17038672749691861649,6290177331721581829

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity tfcmxvabut is
  port (dj : out std_logic_vector(4 downto 0); gsvys : inout integer_value_mirror; ebcw : inout file_value_mirror);
end tfcmxvabut;

architecture k of tfcmxvabut is
  
begin
  -- Multi-driven assignments
  dj <= ('0', '-', 'H', 'X', '1');
  dj <= ('X', 'L', 'H', 'Z', 'W');
  dj <= ('0', 'H', 'X', '-', 'H');
end k;



-- Seed after: 7118802238156364302,6290177331721581829
