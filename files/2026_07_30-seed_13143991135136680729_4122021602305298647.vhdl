-- Seed: 13143991135136680729,4122021602305298647

library ieee;
use ieee.std_logic_1164.all;

entity xirv is
  port (k : out std_logic_vector(4 to 2));
end xirv;

architecture rudxsfhvyj of xirv is
  
begin
  -- Multi-driven assignments
  k <= k;
  k <= k;
  k <= (others => '0');
end rudxsfhvyj;

entity wfqolpanw is
  port (v : inout real);
end wfqolpanw;

library ieee;
use ieee.std_logic_1164.all;

architecture umttuqmvs of wfqolpanw is
  signal kydz : std_logic_vector(4 to 2);
  signal fykve : std_logic_vector(4 to 2);
  signal ei : std_logic_vector(4 to 2);
begin
  ffjhr : entity work.xirv
    port map (k => ei);
  wtrw : entity work.xirv
    port map (k => ei);
  jqqewefaa : entity work.xirv
    port map (k => fykve);
  zjvmoxfo : entity work.xirv
    port map (k => kydz);
  
  -- Single-driven assignments
  v <= v;
  
  -- Multi-driven assignments
  ei <= ei;
  ei <= (others => '0');
end umttuqmvs;

library ieee;
use ieee.std_logic_1164.all;

entity opnhu is
  port (i : inout time; bdm : buffer time; fr : linkage severity_level; xpswuc : in std_logic_vector(1 downto 1));
end opnhu;

library ieee;
use ieee.std_logic_1164.all;

architecture nurt of opnhu is
  signal dey : real;
  signal omcttx : std_logic_vector(4 to 2);
begin
  cicapemghd : entity work.xirv
    port map (k => omcttx);
  eicxsytr : entity work.xirv
    port map (k => omcttx);
  rj : entity work.xirv
    port map (k => omcttx);
  sbhrgikhom : entity work.wfqolpanw
    port map (v => dey);
  
  -- Single-driven assignments
  bdm <= 16#225# ns;
  i <= bdm;
end nurt;



-- Seed after: 6403237933929798008,4122021602305298647
