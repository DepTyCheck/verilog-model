-- Seed: 2107686327750925247,12260394286515585877

library ieee;
use ieee.std_logic_1164.all;

entity gpo is
  port (dsrchia : out time; ualrerbne : inout std_logic; u : inout bit_vector(0 downto 1));
end gpo;

architecture s of gpo is
  
begin
  -- Single-driven assignments
  u <= (others => '0');
  dsrchia <= 1 hr;
  
  -- Multi-driven assignments
  ualrerbne <= ualrerbne;
  ualrerbne <= ualrerbne;
  ualrerbne <= ualrerbne;
  ualrerbne <= ualrerbne;
end s;



-- Seed after: 3316769794409000413,12260394286515585877
