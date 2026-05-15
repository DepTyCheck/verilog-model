-- Seed: 12780540261794914196,1420300365443511127

library ieee;
use ieee.std_logic_1164.all;

entity ghfgtdv is
  port (umprulsgkb : inout time; kqubzquqa : in std_logic; osvy : in time; bvlsp : linkage time);
end ghfgtdv;



architecture ykupmzju of ghfgtdv is
  
begin
  
end ykupmzju;

library ieee;
use ieee.std_logic_1164.all;

entity qd is
  port (uohzzhbi : linkage time; hggujymvah : inout std_logic; dwyvb : out real);
end qd;



architecture zjwqheshb of qd is
  signal egtxxiba : time;
  signal rltozsifaj : time;
  signal vcm : time;
begin
  edqfa : entity work.ghfgtdv
    port map (umprulsgkb => vcm, kqubzquqa => hggujymvah, osvy => rltozsifaj, bvlsp => uohzzhbi);
  b : entity work.ghfgtdv
    port map (umprulsgkb => rltozsifaj, kqubzquqa => hggujymvah, osvy => egtxxiba, bvlsp => uohzzhbi);
  djbcfyd : entity work.ghfgtdv
    port map (umprulsgkb => egtxxiba, kqubzquqa => hggujymvah, osvy => rltozsifaj, bvlsp => vcm);
end zjwqheshb;



entity os is
  port (gwjfxohxo : in integer);
end os;

library ieee;
use ieee.std_logic_1164.all;

architecture flxejiy of os is
  signal zsgau : real;
  signal rkqmzp : std_logic;
  signal bm : time;
  signal soqxhcsrv : std_logic;
  signal qnsbzyihds : time;
begin
  jitqnxot : entity work.ghfgtdv
    port map (umprulsgkb => qnsbzyihds, kqubzquqa => soqxhcsrv, osvy => bm, bvlsp => qnsbzyihds);
  tcndryuyas : entity work.qd
    port map (uohzzhbi => qnsbzyihds, hggujymvah => rkqmzp, dwyvb => zsgau);
end flxejiy;



entity tepzje is
  port (zjorlqc : out character);
end tepzje;

library ieee;
use ieee.std_logic_1164.all;

architecture ozxduzksrc of tepzje is
  signal xxqtzdcney : std_logic;
  signal elqvi : time;
begin
  orofpnks : entity work.ghfgtdv
    port map (umprulsgkb => elqvi, kqubzquqa => xxqtzdcney, osvy => elqvi, bvlsp => elqvi);
end ozxduzksrc;



-- Seed after: 893210653725112728,1420300365443511127
