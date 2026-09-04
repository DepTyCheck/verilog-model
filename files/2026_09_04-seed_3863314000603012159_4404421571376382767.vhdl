-- Seed: 3863314000603012159,4404421571376382767

library ieee;
use ieee.std_logic_1164.all;

entity wk is
  port (plh : buffer std_logic);
end wk;

architecture ib of wk is
  
begin
  -- Multi-driven assignments
  plh <= 'H';
  plh <= '-';
  plh <= 'U';
end ib;



-- Seed after: 13866166790227572278,4404421571376382767
