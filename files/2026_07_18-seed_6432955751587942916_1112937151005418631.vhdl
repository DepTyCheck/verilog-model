-- Seed: 6432955751587942916,1112937151005418631

library ieee;
use ieee.std_logic_1164.all;

entity gummtuti is
  port (pnejnij : inout std_logic);
end gummtuti;

architecture dw of gummtuti is
  
begin
  -- Multi-driven assignments
  pnejnij <= 'H';
  pnejnij <= pnejnij;
end dw;



-- Seed after: 8832960386049556300,1112937151005418631
