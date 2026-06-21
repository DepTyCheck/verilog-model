-- Seed: 3404069216677986988,3687118713772291287

library ieee;
use ieee.std_logic_1164.all;

entity o is
  port (vapp : linkage std_logic; lnd : inout std_logic_vector(4 to 1));
end o;

architecture dq of o is
  
begin
  -- Multi-driven assignments
  lnd <= (others => '0');
end dq;



-- Seed after: 5340558669299334858,3687118713772291287
