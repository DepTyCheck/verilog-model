-- Seed: 16874074818335541545,7311216359267151659

library ieee;
use ieee.std_logic_1164.all;

entity v is
  port (wplhmdzw : inout std_logic);
end v;

architecture ev of v is
  
begin
  -- Multi-driven assignments
  wplhmdzw <= 'L';
  wplhmdzw <= wplhmdzw;
end ev;

library ieee;
use ieee.std_logic_1164.all;

entity qioi is
  port (mxerymnq : out std_logic);
end qioi;

architecture gw of qioi is
  
begin
  cuztf : entity work.v
    port map (wplhmdzw => mxerymnq);
  hkkvkmdqga : entity work.v
    port map (wplhmdzw => mxerymnq);
  ggddiobh : entity work.v
    port map (wplhmdzw => mxerymnq);
  ilw : entity work.v
    port map (wplhmdzw => mxerymnq);
end gw;

library ieee;
use ieee.std_logic_1164.all;

entity m is
  port (npaoosl : out std_logic);
end m;

library ieee;
use ieee.std_logic_1164.all;

architecture h of m is
  signal hplhgttmvk : std_logic;
begin
  fmplh : entity work.v
    port map (wplhmdzw => npaoosl);
  bet : entity work.v
    port map (wplhmdzw => hplhgttmvk);
  lhe : entity work.qioi
    port map (mxerymnq => npaoosl);
  
  -- Multi-driven assignments
  npaoosl <= npaoosl;
  hplhgttmvk <= 'W';
  hplhgttmvk <= npaoosl;
end h;



-- Seed after: 4074996051008774659,7311216359267151659
