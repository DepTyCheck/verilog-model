-- Seed: 4363071976364572817,4122021602305298647

library ieee;
use ieee.std_logic_1164.all;

entity wudo is
  port (wdbf : out time; aaskicnr : out std_logic);
end wudo;

architecture fjay of wudo is
  
begin
  -- Single-driven assignments
  wdbf <= 16#F_C.F_B_8# ns;
  
  -- Multi-driven assignments
  aaskicnr <= aaskicnr;
  aaskicnr <= aaskicnr;
  aaskicnr <= 'H';
  aaskicnr <= aaskicnr;
end fjay;



-- Seed after: 14831795719548800943,4122021602305298647
