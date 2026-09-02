-- Seed: 909012030368162519,3400751927341804175

library ieee;
use ieee.std_logic_1164.all;

entity fb is
  port (rpiz : buffer std_logic);
end fb;

architecture viq of fb is
  
begin
  -- Multi-driven assignments
  rpiz <= rpiz;
end viq;



-- Seed after: 2944265929034365282,3400751927341804175
