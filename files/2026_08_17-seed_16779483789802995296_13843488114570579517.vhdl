-- Seed: 16779483789802995296,13843488114570579517

library ieee;
use ieee.std_logic_1164.all;

entity soe is
  port (ppcfc : inout std_logic; fc : inout real);
end soe;

architecture gborkk of soe is
  
begin
  -- Single-driven assignments
  fc <= 16#DB3AC.3A75#;
  
  -- Multi-driven assignments
  ppcfc <= 'H';
  ppcfc <= ppcfc;
end gborkk;



-- Seed after: 10420113972689880343,13843488114570579517
