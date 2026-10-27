-- Seed: 17653390417556778749,12536735141187299017

library ieee;
use ieee.std_logic_1164.all;

entity twi is
  port (vdbodk : out bit; k : in std_logic_vector(4 downto 0));
end twi;

architecture cppolsnbt of twi is
  
begin
  -- Single-driven assignments
  vdbodk <= '1';
end cppolsnbt;



-- Seed after: 2342861232783789504,12536735141187299017
