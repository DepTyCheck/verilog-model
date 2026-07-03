-- Seed: 6024715904209291749,2158184632809654795

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity wurev is
  port (n : out integer; oj : inout boolean; kbdv : in std_logic; sr : inout record_subtype_mirror);
end wurev;

architecture yv of wurev is
  
begin
  -- Single-driven assignments
  n <= 8#3_6_5_7_6#;
end yv;



-- Seed after: 13710803735767762943,2158184632809654795
