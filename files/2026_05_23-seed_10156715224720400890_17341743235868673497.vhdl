-- Seed: 10156715224720400890,17341743235868673497

library ieee;
use ieee.std_logic_1164.all;

entity ue is
  port (gyrdvbckz : linkage integer; addnda : inout integer; vaumnj : linkage boolean; lvwz : linkage std_logic);
end ue;



architecture budqzto of ue is
  
begin
  
end budqzto;

library ieee;
use ieee.std_logic_1164.all;

entity tmlqt is
  port (orxzj : buffer integer; hydtndu : buffer std_logic; hgqnkfq : in time; kddhdmrddu : linkage integer);
end tmlqt;

library ieee;
use ieee.std_logic_1164.all;

architecture zshvjqdsk of tmlqt is
  signal qbumdfnsed : integer;
  signal nd : std_logic;
  signal gimntx : std_logic;
  signal bihcbzdvq : boolean;
  signal iqousos : integer;
begin
  eixqy : entity work.ue
    port map (gyrdvbckz => orxzj, addnda => iqousos, vaumnj => bihcbzdvq, lvwz => gimntx);
  hlbelzc : entity work.ue
    port map (gyrdvbckz => kddhdmrddu, addnda => orxzj, vaumnj => bihcbzdvq, lvwz => nd);
  arxex : entity work.ue
    port map (gyrdvbckz => qbumdfnsed, addnda => qbumdfnsed, vaumnj => bihcbzdvq, lvwz => hydtndu);
end zshvjqdsk;



entity tub is
  port (mgeob : out real; ilrvo : in severity_level; rrooihm : inout integer);
end tub;

library ieee;
use ieee.std_logic_1164.all;

architecture ygoihh of tub is
  signal hqdykj : boolean;
  signal rmrjvmmfv : integer;
  signal rpcealxn : integer;
  signal tzt : integer;
  signal hireqxb : time;
  signal yyaq : std_logic;
  signal bhuhicphkd : integer;
begin
  dwpzxzjiy : entity work.tmlqt
    port map (orxzj => bhuhicphkd, hydtndu => yyaq, hgqnkfq => hireqxb, kddhdmrddu => tzt);
  olslcluxin : entity work.ue
    port map (gyrdvbckz => rpcealxn, addnda => rmrjvmmfv, vaumnj => hqdykj, lvwz => yyaq);
end ygoihh;



-- Seed after: 15155393355780663910,17341743235868673497
