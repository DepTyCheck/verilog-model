-- Seed: 18057122080955992332,18037650846010261179

library ieee;
use ieee.std_logic_1164.all;

entity cpftthcxw is
  port (tf : inout std_logic; lwqudtvrf : linkage integer; g : in std_logic);
end cpftthcxw;

architecture bgpthbdvos of cpftthcxw is
  
begin
  -- Multi-driven assignments
  tf <= g;
  tf <= 'Z';
end bgpthbdvos;

library ieee;
use ieee.std_logic_1164.all;

entity talzejz is
  port (amx : buffer std_logic);
end talzejz;

library ieee;
use ieee.std_logic_1164.all;

architecture ogzfydgkzm of talzejz is
  signal lanixo : std_logic;
  signal ctnahe : integer;
  signal andnzl : integer;
  signal zj : std_logic;
  signal s : integer;
begin
  tcysn : entity work.cpftthcxw
    port map (tf => amx, lwqudtvrf => s, g => zj);
  hrqxgsebv : entity work.cpftthcxw
    port map (tf => amx, lwqudtvrf => andnzl, g => zj);
  ingyzmllxo : entity work.cpftthcxw
    port map (tf => amx, lwqudtvrf => ctnahe, g => lanixo);
end ogzfydgkzm;

entity p is
  port (hdenoigw : out time);
end p;

library ieee;
use ieee.std_logic_1164.all;

architecture hkbkg of p is
  signal pegutrnt : integer;
  signal c : std_logic;
  signal et : std_logic;
begin
  ykwjwfm : entity work.talzejz
    port map (amx => et);
  uhloulgh : entity work.cpftthcxw
    port map (tf => c, lwqudtvrf => pegutrnt, g => et);
  
  -- Single-driven assignments
  hdenoigw <= hdenoigw;
  
  -- Multi-driven assignments
  et <= '-';
  et <= et;
  c <= et;
end hkbkg;



-- Seed after: 6529303943221524483,18037650846010261179
