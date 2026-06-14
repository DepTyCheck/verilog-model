-- Seed: 3035426862207313849,1852660963590380551

library ieee;
use ieee.std_logic_1164.all;

entity amp is
  port ( n : linkage std_logic_vector(2 downto 0)
  ; nyvp : in std_logic_vector(0 downto 1)
  ; ooc : out std_logic_vector(0 downto 2)
  ; qg : in integer_vector(3 to 4)
  );
end amp;



architecture ifuqalxge of amp is
  
begin
  
end ifuqalxge;

library ieee;
use ieee.std_logic_1164.all;

entity akzvmcxqyu is
  port (bzt : buffer std_logic_vector(4 downto 4); wdnpiasnl : inout std_logic);
end akzvmcxqyu;

library ieee;
use ieee.std_logic_1164.all;

architecture jcofav of akzvmcxqyu is
  signal lgvwhtpd : integer_vector(3 to 4);
  signal rotsd : integer_vector(3 to 4);
  signal ltbfsaur : std_logic_vector(0 downto 2);
  signal x : std_logic_vector(0 downto 2);
  signal fqtrxeuev : std_logic_vector(2 downto 0);
begin
  lvjite : entity work.amp
    port map (n => fqtrxeuev, nyvp => x, ooc => ltbfsaur, qg => rotsd);
  pvwgpo : entity work.amp
    port map (n => fqtrxeuev, nyvp => x, ooc => x, qg => lgvwhtpd);
end jcofav;

library ieee;
use ieee.std_logic_1164.all;

entity yngxdg is
  port (bakottd : buffer real; qqgsvgdtyy : out std_logic; gmuvnaonc : linkage bit);
end yngxdg;

library ieee;
use ieee.std_logic_1164.all;

architecture sfmhxwuw of yngxdg is
  signal adzqttr : integer_vector(3 to 4);
  signal v : std_logic_vector(0 downto 2);
  signal axde : std_logic_vector(0 downto 1);
  signal iuaajoserf : std_logic_vector(2 downto 0);
  signal qwg : std_logic;
  signal ijclin : std_logic_vector(4 downto 4);
begin
  hbisasuy : entity work.akzvmcxqyu
    port map (bzt => ijclin, wdnpiasnl => qwg);
  op : entity work.amp
    port map (n => iuaajoserf, nyvp => axde, ooc => v, qg => adzqttr);
end sfmhxwuw;



entity wfrqhtmq is
  port (t : linkage real; hca : out real; tsshp : out real; lwedkps : inout real_vector(1 to 2));
end wfrqhtmq;

library ieee;
use ieee.std_logic_1164.all;

architecture bxoqcm of wfrqhtmq is
  signal aqjzzxnp : bit;
  signal w : std_logic;
  signal qzcmow : real;
  signal zzuxjbyy : integer_vector(3 to 4);
  signal mz : std_logic_vector(0 downto 1);
  signal fwkngop : integer_vector(3 to 4);
  signal uicns : std_logic_vector(0 downto 2);
  signal ottbno : std_logic_vector(0 downto 2);
  signal znwl : std_logic_vector(2 downto 0);
begin
  etxtnvods : entity work.amp
    port map (n => znwl, nyvp => ottbno, ooc => uicns, qg => fwkngop);
  gtzqhdapu : entity work.amp
    port map (n => znwl, nyvp => mz, ooc => ottbno, qg => zzuxjbyy);
  ysqfpuyths : entity work.yngxdg
    port map (bakottd => qzcmow, qqgsvgdtyy => w, gmuvnaonc => aqjzzxnp);
end bxoqcm;



-- Seed after: 15613528969021099828,1852660963590380551
