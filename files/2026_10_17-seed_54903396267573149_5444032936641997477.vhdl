-- Seed: 54903396267573149,5444032936641997477

library ieee;
use ieee.std_logic_1164.all;

entity i is
  port (yd : buffer std_logic);
end i;

architecture g of i is
  
begin
  -- Multi-driven assignments
  yd <= 'X';
  yd <= yd;
  yd <= yd;
  yd <= yd;
end g;



-- Seed after: 10759448244718340135,5444032936641997477
