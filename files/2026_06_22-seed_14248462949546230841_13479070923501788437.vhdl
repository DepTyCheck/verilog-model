-- Seed: 14248462949546230841,13479070923501788437

library ieee;
use ieee.std_logic_1164.all;

entity vubds is
  port (mvhlc : buffer severity_level; bd : inout real; thgxl : buffer std_logic_vector(2 to 2); sswln : linkage time);
end vubds;

architecture cnmeb of vubds is
  
begin
  -- Single-driven assignments
  mvhlc <= WARNING;
  bd <= 1_1.3_4;
  
  -- Multi-driven assignments
  thgxl <= "1";
  thgxl <= (others => '1');
  thgxl <= "-";
end cnmeb;



-- Seed after: 16274398269188369846,13479070923501788437
