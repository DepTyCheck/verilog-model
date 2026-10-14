-- Seed: 14104388666753549798,2287986438313874379

library ieee;
use ieee.std_logic_1164.all;

entity n is
  port (bakavv : inout std_logic_vector(4 to 0); z : in std_logic);
end n;

architecture m of n is
  
begin
  -- Multi-driven assignments
  bakavv <= "";
  bakavv <= bakavv;
end m;



-- Seed after: 18265876020607443014,2287986438313874379
