-- Seed: 11800061717174605015,8089241273282434469

library ieee;
use ieee.std_logic_1164.all;

entity ruso is
  port (kkgomf : buffer time_vector(0 to 4); omkqt : linkage time; epgiz : out std_logic_vector(2 to 0); hixd : inout std_logic_vector(0 downto 0));
end ruso;



architecture hlfwyazqdx of ruso is
  
begin
  
end hlfwyazqdx;

library ieee;
use ieee.std_logic_1164.all;

entity ov is
  port (koz : linkage std_logic; ixhe : linkage character);
end ov;

library ieee;
use ieee.std_logic_1164.all;

architecture x of ov is
  signal kphs : time_vector(0 to 4);
  signal xqqhvuku : time_vector(0 to 4);
  signal imtjxjvis : std_logic_vector(0 downto 0);
  signal entyfbtak : std_logic_vector(2 to 0);
  signal ixmfopgf : time_vector(0 to 4);
  signal pkatc : std_logic_vector(0 downto 0);
  signal veey : std_logic_vector(2 to 0);
  signal yexy : time;
  signal aeg : time_vector(0 to 4);
begin
  cwtnluy : entity work.ruso
    port map (kkgomf => aeg, omkqt => yexy, epgiz => veey, hixd => pkatc);
  foaqxmyd : entity work.ruso
    port map (kkgomf => ixmfopgf, omkqt => yexy, epgiz => entyfbtak, hixd => imtjxjvis);
  uww : entity work.ruso
    port map (kkgomf => xqqhvuku, omkqt => yexy, epgiz => veey, hixd => pkatc);
  anc : entity work.ruso
    port map (kkgomf => kphs, omkqt => yexy, epgiz => veey, hixd => pkatc);
end x;

library ieee;
use ieee.std_logic_1164.all;

entity q is
  port (dlypta : buffer std_logic_vector(0 to 4); tiwz : buffer boolean_vector(3 to 2); npmvxd : linkage integer);
end q;

library ieee;
use ieee.std_logic_1164.all;

architecture zfafzitf of q is
  signal ugldbviqe : character;
  signal gvnqzk : std_logic;
  signal gsldfajlej : character;
  signal rgubij : std_logic;
begin
  yajnlhjs : entity work.ov
    port map (koz => rgubij, ixhe => gsldfajlej);
  uoryg : entity work.ov
    port map (koz => gvnqzk, ixhe => ugldbviqe);
end zfafzitf;

library ieee;
use ieee.std_logic_1164.all;

entity qrahpf is
  port (yxfteerqo : out time; tuvracg : in std_logic_vector(2 to 1); ooempqn : inout bit_vector(4 to 1); ipo : linkage real);
end qrahpf;

library ieee;
use ieee.std_logic_1164.all;

architecture m of qrahpf is
  signal hc : boolean_vector(3 to 2);
  signal cwyf : character;
  signal anisllt : integer;
  signal kr : boolean_vector(3 to 2);
  signal leerqskwf : std_logic_vector(0 to 4);
  signal ukk : character;
  signal ufnkxwjgj : std_logic;
begin
  hubhj : entity work.ov
    port map (koz => ufnkxwjgj, ixhe => ukk);
  g : entity work.q
    port map (dlypta => leerqskwf, tiwz => kr, npmvxd => anisllt);
  ogldib : entity work.ov
    port map (koz => ufnkxwjgj, ixhe => cwyf);
  rnqpmxf : entity work.q
    port map (dlypta => leerqskwf, tiwz => hc, npmvxd => anisllt);
end m;



-- Seed after: 11613532078582305270,8089241273282434469
