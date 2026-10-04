-- Seed: 8367209512826407054,15795020531041709203

library ieee;
use ieee.std_logic_1164.all;

entity rohosi is
  port (zp : inout integer; zgfjss : inout std_logic);
end rohosi;

architecture hve of rohosi is
  
begin
  -- Single-driven assignments
  zp <= 2#0#;
  
  -- Multi-driven assignments
  zgfjss <= 'H';
  zgfjss <= zgfjss;
end hve;



-- Seed after: 5596899587517724806,15795020531041709203
