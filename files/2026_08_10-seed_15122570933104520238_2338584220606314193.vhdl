-- Seed: 15122570933104520238,2338584220606314193

library ieee;
use ieee.std_logic_1164.all;

entity y is
  port (qunl : out boolean; zbe : buffer std_logic);
end y;

architecture m of y is
  
begin
  -- Single-driven assignments
  qunl <= FALSE;
  
  -- Multi-driven assignments
  zbe <= 'W';
  zbe <= zbe;
  zbe <= 'H';
end m;



-- Seed after: 14562177855629752362,2338584220606314193
