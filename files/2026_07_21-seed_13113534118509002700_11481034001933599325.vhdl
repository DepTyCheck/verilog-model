-- Seed: 13113534118509002700,11481034001933599325

library ieee;
use ieee.std_logic_1164.all;

entity ql is
  port (tin : in time; l : inout std_logic; tkulqa : in time);
end ql;

architecture fg of ql is
  
begin
  -- Multi-driven assignments
  l <= 'W';
  l <= '1';
  l <= '0';
  l <= 'Z';
end fg;



-- Seed after: 14215604862293721594,11481034001933599325
