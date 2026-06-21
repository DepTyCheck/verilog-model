-- Seed: 2263729793823745840,3687118713772291287

library ieee;
use ieee.std_logic_1164.all;

entity r is
  port (wzg : linkage integer; cogiih : inout std_logic_vector(0 to 0); nwaj : inout boolean; gxlhfdtl : out string(4 to 3));
end r;

architecture c of r is
  
begin
  -- Single-driven assignments
  nwaj <= FALSE;
  
  -- Multi-driven assignments
  cogiih <= (others => 'L');
  cogiih <= (others => 'H');
  cogiih <= "U";
  cogiih <= "X";
end c;



-- Seed after: 18000901189446168473,3687118713772291287
