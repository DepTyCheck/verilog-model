-- Seed: 4847896137615799786,14426950258250697445

library ieee;
use ieee.std_logic_1164.all;

entity unxi is
  port (hn : buffer integer; vbzb : inout std_logic);
end unxi;

architecture b of unxi is
  
begin
  -- Single-driven assignments
  hn <= 8#56541#;
  
  -- Multi-driven assignments
  vbzb <= vbzb;
  vbzb <= 'L';
  vbzb <= vbzb;
  vbzb <= vbzb;
end b;



-- Seed after: 3455946487318284949,14426950258250697445
