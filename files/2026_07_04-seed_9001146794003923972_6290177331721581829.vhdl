-- Seed: 9001146794003923972,6290177331721581829

entity oti is
  port (bwgfadbeq : buffer time);
end oti;

architecture e of oti is
  
begin
  -- Single-driven assignments
  bwgfadbeq <= 1 sec;
end e;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity tbich is
  port (gwzrpaypcn : inout std_logic; vkjeechlvc : inout protected_value_mirror);
end tbich;

architecture nji of tbich is
  signal skzcb : time;
begin
  oda : entity work.oti
    port map (bwgfadbeq => skzcb);
  
  -- Multi-driven assignments
  gwzrpaypcn <= 'W';
  gwzrpaypcn <= '1';
  gwzrpaypcn <= 'Z';
  gwzrpaypcn <= '-';
end nji;

use std.reflection.all;

entity z is
  port (xomsedjq : inout integer_value_mirror; r : inout integer_value_mirror);
end z;

architecture nyy of z is
  
begin
  
end nyy;

library ieee;
use ieee.std_logic_1164.all;

entity rkjlbstb is
  port (dn : out real; li : buffer std_logic);
end rkjlbstb;

architecture g of rkjlbstb is
  
begin
  -- Single-driven assignments
  dn <= dn;
  
  -- Multi-driven assignments
  li <= '-';
  li <= '-';
  li <= 'Z';
  li <= 'H';
end g;



-- Seed after: 6837566396529185646,6290177331721581829
