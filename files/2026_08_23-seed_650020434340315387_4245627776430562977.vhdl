-- Seed: 650020434340315387,4245627776430562977

library ieee;
use ieee.std_logic_1164.all;

entity gzmatds is
  port (nllarxirf : inout std_logic; knsw : in integer);
end gzmatds;

architecture bxf of gzmatds is
  
begin
  -- Multi-driven assignments
  nllarxirf <= 'W';
  nllarxirf <= 'X';
  nllarxirf <= nllarxirf;
  nllarxirf <= nllarxirf;
end bxf;



-- Seed after: 8367050505588390034,4245627776430562977
