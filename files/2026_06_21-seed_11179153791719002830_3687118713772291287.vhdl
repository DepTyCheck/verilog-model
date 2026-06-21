-- Seed: 11179153791719002830,3687118713772291287

library ieee;
use ieee.std_logic_1164.all;

entity rwlv is
  port (ihb : buffer time; gx : inout std_logic);
end rwlv;

architecture bexbnxn of rwlv is
  
begin
  -- Single-driven assignments
  ihb <= 3_3 us;
end bexbnxn;



-- Seed after: 1887029530302049124,3687118713772291287
