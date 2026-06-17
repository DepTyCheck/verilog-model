-- Seed: 10240305563029907222,10557070023141912087

library ieee;
use ieee.std_logic_1164.all;

entity wmmb is
  port (bylfrsc : buffer std_logic; eb : inout character);
end wmmb;

architecture bdo of wmmb is
  
begin
  -- Single-driven assignments
  eb <= 'k';
end bdo;



-- Seed after: 9954934290159209252,10557070023141912087
