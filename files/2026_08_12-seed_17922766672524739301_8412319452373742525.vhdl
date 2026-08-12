-- Seed: 17922766672524739301,8412319452373742525

library ieee;
use ieee.std_logic_1164.all;

entity focgfrpid is
  port (mgocr : linkage real; theq : linkage character; rhudiv : inout std_logic);
end focgfrpid;

architecture vc of focgfrpid is
  
begin
  -- Multi-driven assignments
  rhudiv <= 'X';
  rhudiv <= rhudiv;
  rhudiv <= '1';
  rhudiv <= rhudiv;
end vc;



-- Seed after: 12642087973990280531,8412319452373742525
