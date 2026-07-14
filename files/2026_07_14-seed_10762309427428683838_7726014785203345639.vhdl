-- Seed: 10762309427428683838,7726014785203345639

library ieee;
use ieee.std_logic_1164.all;

entity uulqnpt is
  port (vty : buffer std_logic_vector(0 to 0); gulspmc : linkage std_logic_vector(0 downto 2));
end uulqnpt;

architecture z of uulqnpt is
  
begin
  -- Multi-driven assignments
  vty <= vty;
  vty <= (others => 'W');
  vty <= "1";
  vty <= vty;
end z;

use std.reflection.all;

entity qktmfp is
  port (lfqoc : inout bit_vector(1 to 1); n : inout record_subtype_mirror; rrjbhcbfuz : buffer integer);
end qktmfp;

library ieee;
use ieee.std_logic_1164.all;

architecture zyrq of qktmfp is
  signal lonsp : std_logic_vector(0 downto 2);
  signal xivrfp : std_logic_vector(0 downto 2);
  signal isuywxba : std_logic_vector(0 to 0);
begin
  ogkcjlqqgw : entity work.uulqnpt
    port map (vty => isuywxba, gulspmc => xivrfp);
  lcxqdbsfke : entity work.uulqnpt
    port map (vty => isuywxba, gulspmc => lonsp);
  tfjquqpw : entity work.uulqnpt
    port map (vty => isuywxba, gulspmc => xivrfp);
  
  -- Single-driven assignments
  lfqoc <= lfqoc;
  
  -- Multi-driven assignments
  isuywxba <= (others => 'U');
end zyrq;

use std.reflection.all;

entity othdlxt is
  port (rblp : inout protected_value_mirror);
end othdlxt;

library ieee;
use ieee.std_logic_1164.all;

architecture nemhwntxca of othdlxt is
  signal ufmprj : std_logic_vector(0 downto 2);
  signal kz : std_logic_vector(0 downto 2);
  signal zq : std_logic_vector(0 to 0);
begin
  aqtwcywug : entity work.uulqnpt
    port map (vty => zq, gulspmc => kz);
  rappqbc : entity work.uulqnpt
    port map (vty => zq, gulspmc => ufmprj);
end nemhwntxca;



-- Seed after: 489074898284686637,7726014785203345639
