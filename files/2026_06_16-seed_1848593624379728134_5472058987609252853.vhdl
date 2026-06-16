-- Seed: 1848593624379728134,5472058987609252853

library ieee;
use ieee.std_logic_1164.all;

entity avsgdpc is
  port (teehq : buffer std_logic_vector(2 downto 0); mvxki : in integer; xcuqiciki : buffer std_logic; usc : inout std_logic_vector(0 to 1));
end avsgdpc;

architecture wybcx of avsgdpc is
  
begin
  -- Multi-driven assignments
  teehq <= "XXX";
  usc <= "L0";
  teehq <= "-H-";
end wybcx;

library ieee;
use ieee.std_logic_1164.all;

entity whodlpi is
  port (dobgdelx : inout std_logic);
end whodlpi;

library ieee;
use ieee.std_logic_1164.all;

architecture cbduemhpg of whodlpi is
  signal whocerej : std_logic;
  signal zcvl : std_logic_vector(2 downto 0);
  signal cchehqopth : std_logic_vector(0 to 1);
  signal huofsspfn : integer;
  signal egxgcz : std_logic_vector(2 downto 0);
begin
  aehmocx : entity work.avsgdpc
    port map (teehq => egxgcz, mvxki => huofsspfn, xcuqiciki => dobgdelx, usc => cchehqopth);
  dphrbh : entity work.avsgdpc
    port map (teehq => zcvl, mvxki => huofsspfn, xcuqiciki => whocerej, usc => cchehqopth);
  
  -- Single-driven assignments
  huofsspfn <= 0_2;
  
  -- Multi-driven assignments
  egxgcz <= "HL1";
end cbduemhpg;

library ieee;
use ieee.std_logic_1164.all;

entity nfqec is
  port (hnihglwxe : buffer integer; usyfjxygeb : buffer bit; ngray : in std_logic_vector(1 downto 4); djaaner : in integer);
end nfqec;

library ieee;
use ieee.std_logic_1164.all;

architecture ou of nfqec is
  signal qegc : std_logic;
  signal vstfl : std_logic_vector(0 to 1);
  signal gpi : std_logic;
  signal uuasad : integer;
  signal elikkpuqoh : std_logic_vector(0 to 1);
  signal ueftze : std_logic;
  signal lisolx : std_logic_vector(2 downto 0);
begin
  nwttbbpnv : entity work.avsgdpc
    port map (teehq => lisolx, mvxki => djaaner, xcuqiciki => ueftze, usc => elikkpuqoh);
  l : entity work.avsgdpc
    port map (teehq => lisolx, mvxki => uuasad, xcuqiciki => gpi, usc => vstfl);
  bp : entity work.whodlpi
    port map (dobgdelx => ueftze);
  iyztwnvmfg : entity work.avsgdpc
    port map (teehq => lisolx, mvxki => djaaner, xcuqiciki => qegc, usc => elikkpuqoh);
  
  -- Single-driven assignments
  usyfjxygeb <= '0';
  hnihglwxe <= 2#111#;
  uuasad <= 8#7_4_3_4#;
  
  -- Multi-driven assignments
  vstfl <= ('1', 'H');
end ou;

entity jtnhvb is
  port (b : inout real; mpbtly : in severity_level);
end jtnhvb;

library ieee;
use ieee.std_logic_1164.all;

architecture gefppn of jtnhvb is
  signal r : std_logic_vector(0 to 1);
  signal qkfgurc : std_logic_vector(2 downto 0);
  signal fqjh : std_logic_vector(0 to 1);
  signal em : std_logic;
  signal kawdjr : integer;
  signal jv : std_logic_vector(2 downto 0);
begin
  tqxjzztsl : entity work.avsgdpc
    port map (teehq => jv, mvxki => kawdjr, xcuqiciki => em, usc => fqjh);
  tuyajepc : entity work.avsgdpc
    port map (teehq => qkfgurc, mvxki => kawdjr, xcuqiciki => em, usc => r);
  
  -- Single-driven assignments
  b <= 3_4_4_1.431;
  kawdjr <= 16#1FA#;
  
  -- Multi-driven assignments
  jv <= "L11";
end gefppn;



-- Seed after: 10576604187652065356,5472058987609252853
