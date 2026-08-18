-- Seed: 12630704505094718352,5983430343285687595

library ieee;
use ieee.std_logic_1164.all;

entity bkxnoug is
  port (q : inout severity_level; acw : inout std_logic);
end bkxnoug;

architecture ae of bkxnoug is
  
begin
  -- Single-driven assignments
  q <= FAILURE;
  
  -- Multi-driven assignments
  acw <= '-';
end ae;



-- Seed after: 7185432760605650858,5983430343285687595
