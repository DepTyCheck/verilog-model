-- Seed: 12510702491372223556,8437298063418820479

library ieee;
use ieee.std_logic_1164.all;

entity aljg is
  port (er : inout std_logic);
end aljg;

architecture bs of aljg is
  
begin
  -- Multi-driven assignments
  er <= er;
  er <= '-';
  er <= er;
end bs;



-- Seed after: 15138234800595070994,8437298063418820479
