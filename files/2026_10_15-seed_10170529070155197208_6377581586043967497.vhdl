-- Seed: 10170529070155197208,6377581586043967497

library ieee;
use ieee.std_logic_1164.all;

entity ub is
  port (bmupa : linkage integer; dbyceraf : buffer std_logic_vector(1 to 3); gcehzah : inout bit_vector(1 to 4));
end ub;

architecture pt of ub is
  
begin
  -- Single-driven assignments
  gcehzah <= ('1', '1', '1', '1');
  
  -- Multi-driven assignments
  dbyceraf <= ('H', '1', 'L');
  dbyceraf <= dbyceraf;
  dbyceraf <= dbyceraf;
end pt;



-- Seed after: 10463095659753832450,6377581586043967497
