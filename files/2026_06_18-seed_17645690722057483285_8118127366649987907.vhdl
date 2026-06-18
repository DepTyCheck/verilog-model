-- Seed: 17645690722057483285,8118127366649987907

library ieee;
use ieee.std_logic_1164.all;

entity cx is
  port (q : out std_logic_vector(2 downto 1); hcdwoc : out std_logic_vector(3 to 2));
end cx;

architecture xm of cx is
  
begin
  -- Multi-driven assignments
  q <= "HH";
  hcdwoc <= (others => '0');
  hcdwoc <= "";
  hcdwoc <= "";
end xm;



-- Seed after: 2735414073874924037,8118127366649987907
