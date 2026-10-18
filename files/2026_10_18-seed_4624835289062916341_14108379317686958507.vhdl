-- Seed: 4624835289062916341,14108379317686958507

library ieee;
use ieee.std_logic_1164.all;

entity lucv is
  port (gwzmesxl : buffer std_logic_vector(1 to 3); awozw : in integer);
end lucv;

architecture ihdgktip of lucv is
  
begin
  -- Multi-driven assignments
  gwzmesxl <= ('W', 'X', 'X');
  gwzmesxl <= "H01";
end ihdgktip;

library ieee;
use ieee.std_logic_1164.all;

entity v is
  port (a : buffer std_logic; ue : inout integer);
end v;

library ieee;
use ieee.std_logic_1164.all;

architecture lyl of v is
  signal i : std_logic_vector(1 to 3);
  signal srbd : integer;
  signal nzvutuc : std_logic_vector(1 to 3);
begin
  iky : entity work.lucv
    port map (gwzmesxl => nzvutuc, awozw => srbd);
  itspucptdk : entity work.lucv
    port map (gwzmesxl => i, awozw => ue);
  
  -- Single-driven assignments
  ue <= 3_3_2;
  srbd <= 2#0_0_1_0#;
  
  -- Multi-driven assignments
  nzvutuc <= nzvutuc;
end lyl;



-- Seed after: 11177864803979502448,14108379317686958507
