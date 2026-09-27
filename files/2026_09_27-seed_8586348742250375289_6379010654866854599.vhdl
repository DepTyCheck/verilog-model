-- Seed: 8586348742250375289,6379010654866854599

library ieee;
use ieee.std_logic_1164.all;

entity rf is
  port (g : out integer; xyrt : out std_logic_vector(1 to 3); d : inout std_logic_vector(1 to 0); jje : out std_logic_vector(1 to 0));
end rf;

architecture pmc of rf is
  
begin
  -- Single-driven assignments
  g <= 43;
  
  -- Multi-driven assignments
  xyrt <= ('-', 'X', '-');
  xyrt <= ('-', '1', 'X');
  jje <= d;
  jje <= jje;
end pmc;



-- Seed after: 12964644936273966989,6379010654866854599
