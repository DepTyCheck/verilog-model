-- Seed: 3415830692869600374,5444032936641997477

library ieee;
use ieee.std_logic_1164.all;

entity kly is
  port (bedmi : inout std_logic);
end kly;

architecture hq of kly is
  
begin
  -- Multi-driven assignments
  bedmi <= 'X';
  bedmi <= bedmi;
end hq;



-- Seed after: 7776439749969689805,5444032936641997477
