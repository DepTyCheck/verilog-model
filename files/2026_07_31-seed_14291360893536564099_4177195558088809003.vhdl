-- Seed: 14291360893536564099,4177195558088809003

library ieee;
use ieee.std_logic_1164.all;

entity kpd is
  port (xlir : in std_logic; ois : buffer time);
end kpd;

architecture uvel of kpd is
  
begin
  -- Single-driven assignments
  ois <= 3230.02 us;
end uvel;



-- Seed after: 173365394397985744,4177195558088809003
