-- Seed: 11295799649561289538,12536735141187299017

library ieee;
use ieee.std_logic_1164.all;

entity gsam is
  port (vhqxgps : buffer std_logic_vector(4 downto 4); ln : out boolean_vector(0 to 0));
end gsam;

architecture bg of gsam is
  
begin
  -- Single-driven assignments
  ln <= (others => TRUE);
  
  -- Multi-driven assignments
  vhqxgps <= (others => 'X');
  vhqxgps <= "X";
end bg;



-- Seed after: 9990343306130743841,12536735141187299017
