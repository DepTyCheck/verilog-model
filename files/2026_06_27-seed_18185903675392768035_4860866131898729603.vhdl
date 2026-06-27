-- Seed: 18185903675392768035,4860866131898729603

library ieee;
use ieee.std_logic_1164.all;

entity jhrayjdf is
  port (jmuca : out std_logic_vector(2 to 0); ersi : buffer bit; rntl : inout real; n : inout time);
end jhrayjdf;

architecture ld of jhrayjdf is
  
begin
  -- Single-driven assignments
  rntl <= 4.01;
  ersi <= '1';
  n <= 16#E_A# ps;
  
  -- Multi-driven assignments
  jmuca <= "";
  jmuca <= (others => '0');
end ld;



-- Seed after: 16765734702524811125,4860866131898729603
