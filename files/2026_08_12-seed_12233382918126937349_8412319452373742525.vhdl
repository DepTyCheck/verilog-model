-- Seed: 12233382918126937349,8412319452373742525

library ieee;
use ieee.std_logic_1164.all;

entity xoduggy is
  port (rfo : inout severity_level; gbjs : buffer integer; vpr : buffer time; j : out std_logic_vector(2 to 2));
end xoduggy;

architecture cr of xoduggy is
  
begin
  -- Single-driven assignments
  vpr <= vpr;
  rfo <= NOTE;
  gbjs <= gbjs;
end cr;



-- Seed after: 10762883986771786523,8412319452373742525
