-- Seed: 15964334365882308234,12011142928354116943

library ieee;
use ieee.std_logic_1164.all;

entity n is
  port (raaltuma : inout std_logic_vector(4 downto 0); ng : out time; hyz : linkage time);
end n;

architecture nd of n is
  
begin
  -- Single-driven assignments
  ng <= 34132 ms;
  
  -- Multi-driven assignments
  raaltuma <= "0HZL1";
  raaltuma <= ('1', 'X', 'X', 'W', 'H');
end nd;

entity wkgvxym is
  port (uv : linkage integer; v : out severity_level);
end wkgvxym;

library ieee;
use ieee.std_logic_1164.all;

architecture bwcbz of wkgvxym is
  signal kesqgcroy : time;
  signal jcshryz : time;
  signal udigraedng : time;
  signal hnzfys : time;
  signal hcag : std_logic_vector(4 downto 0);
  signal hjbxna : time;
  signal evibldcvo : time;
  signal ypahz : time;
  signal ibvkfurmou : time;
  signal psc : std_logic_vector(4 downto 0);
begin
  bmbjlsyu : entity work.n
    port map (raaltuma => psc, ng => ibvkfurmou, hyz => ypahz);
  mapcmihocs : entity work.n
    port map (raaltuma => psc, ng => evibldcvo, hyz => hjbxna);
  vrrwc : entity work.n
    port map (raaltuma => hcag, ng => hnzfys, hyz => udigraedng);
  ucn : entity work.n
    port map (raaltuma => psc, ng => jcshryz, hyz => kesqgcroy);
  
  -- Single-driven assignments
  v <= ERROR;
  
  -- Multi-driven assignments
  psc <= ('H', 'L', 'U', 'W', 'H');
end bwcbz;

entity d is
  port (yrnumswl : inout real; qcpg : in time; u : inout integer);
end d;

architecture mfjks of d is
  signal nnlnzv : severity_level;
  signal bpcawa : integer;
begin
  vmouju : entity work.wkgvxym
    port map (uv => bpcawa, v => nnlnzv);
  
  -- Single-driven assignments
  u <= 8#23664#;
  yrnumswl <= 1.3_3_1;
end mfjks;



-- Seed after: 15603592469120648506,12011142928354116943
