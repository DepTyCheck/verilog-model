-- Seed: 14333273858159832508,2158184632809654795

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity vhvn is
  port (hjruud : inout std_logic; lrtxkq : inout record_value_mirror);
end vhvn;

architecture kvdwuddtcr of vhvn is
  
begin
  -- Multi-driven assignments
  hjruud <= hjruud;
  hjruud <= 'W';
  hjruud <= '-';
  hjruud <= '1';
end kvdwuddtcr;

library ieee;
use ieee.std_logic_1164.all;

entity gzlh is
  port (jv : out boolean; ngkj : inout std_logic);
end gzlh;

use std.reflection.all;

architecture uqdt of gzlh is
  shared variable tr : record_value_mirror;
begin
  vwwcietr : entity work.vhvn
    port map (hjruud => ngkj, lrtxkq => tr);
  
  -- Single-driven assignments
  jv <= TRUE;
  
  -- Multi-driven assignments
  ngkj <= ngkj;
  ngkj <= '-';
  ngkj <= '-';
  ngkj <= 'L';
end uqdt;



-- Seed after: 13142679281159485779,2158184632809654795
