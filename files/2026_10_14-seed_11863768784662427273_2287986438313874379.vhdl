-- Seed: 11863768784662427273,2287986438313874379

library ieee;
use ieee.std_logic_1164.all;

entity rmhw is
  port (kcleoncwif : inout std_logic_vector(4 to 3));
end rmhw;

architecture o of rmhw is
  
begin
  -- Multi-driven assignments
  kcleoncwif <= "";
  kcleoncwif <= kcleoncwif;
  kcleoncwif <= kcleoncwif;
  kcleoncwif <= kcleoncwif;
end o;



-- Seed after: 4831372492172142718,2287986438313874379
