-- Seed: 7009129922450336320,17219977430298397867

library ieee;
use ieee.std_logic_1164.all;

entity nzh is
  port (n : out std_logic);
end nzh;

architecture ink of nzh is
  
begin
  -- Multi-driven assignments
  n <= '0';
  n <= n;
end ink;



-- Seed after: 14723996547699671846,17219977430298397867
