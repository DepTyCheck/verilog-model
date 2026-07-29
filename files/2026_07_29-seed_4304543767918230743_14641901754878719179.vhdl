-- Seed: 4304543767918230743,14641901754878719179

library ieee;
use ieee.std_logic_1164.all;

entity opzl is
  port (oastfvlh : buffer std_logic; yiwzvyuit : out std_logic; yyzp : buffer integer);
end opzl;

architecture rlghnqk of opzl is
  
begin
  -- Single-driven assignments
  yyzp <= yyzp;
  
  -- Multi-driven assignments
  yiwzvyuit <= oastfvlh;
  yiwzvyuit <= yiwzvyuit;
  yiwzvyuit <= 'H';
end rlghnqk;

library ieee;
use ieee.std_logic_1164.all;

entity nrijghuokb is
  port (vwsh : inout std_logic; hptj : inout boolean; snivlzla : in bit);
end nrijghuokb;

library ieee;
use ieee.std_logic_1164.all;

architecture jhsmqjmhtx of nrijghuokb is
  signal xvvo : integer;
  signal wa : std_logic;
  signal mcbwtojz : integer;
  signal srshntibff : std_logic;
  signal lvibocy : std_logic;
  signal aqprubeave : integer;
  signal cnwwixve : std_logic;
begin
  spgdukqk : entity work.opzl
    port map (oastfvlh => vwsh, yiwzvyuit => cnwwixve, yyzp => aqprubeave);
  ckrxan : entity work.opzl
    port map (oastfvlh => lvibocy, yiwzvyuit => srshntibff, yyzp => mcbwtojz);
  ql : entity work.opzl
    port map (oastfvlh => cnwwixve, yiwzvyuit => wa, yyzp => xvvo);
  
  -- Single-driven assignments
  hptj <= FALSE;
  
  -- Multi-driven assignments
  wa <= '0';
end jhsmqjmhtx;

library ieee;
use ieee.std_logic_1164.all;

entity hgtwovilc is
  port (pshnl : inout std_logic_vector(1 downto 0));
end hgtwovilc;

library ieee;
use ieee.std_logic_1164.all;

architecture jyoicuqsq of hgtwovilc is
  signal gwc : boolean;
  signal bcforzgc : bit;
  signal hzsesk : boolean;
  signal xcj : std_logic;
begin
  xniyzx : entity work.nrijghuokb
    port map (vwsh => xcj, hptj => hzsesk, snivlzla => bcforzgc);
  pooefstaf : entity work.nrijghuokb
    port map (vwsh => xcj, hptj => gwc, snivlzla => bcforzgc);
  
  -- Single-driven assignments
  bcforzgc <= bcforzgc;
  
  -- Multi-driven assignments
  pshnl <= ('Z', 'X');
  pshnl <= "H0";
  pshnl <= pshnl;
  pshnl <= "LL";
end jyoicuqsq;



-- Seed after: 8085317181697787343,14641901754878719179
