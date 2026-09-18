-- Seed: 13109892490084070634,3316342841050048249

library ieee;
use ieee.std_logic_1164.all;

entity gsb is
  port (ujsn : buffer std_logic; z : in integer);
end gsb;

architecture fase of gsb is
  
begin
  -- Multi-driven assignments
  ujsn <= 'H';
  ujsn <= ujsn;
  ujsn <= 'W';
  ujsn <= ujsn;
end fase;



-- Seed after: 4047766825584285618,3316342841050048249
