-- Seed: 864932289940800977,2511821214772927453

library ieee;
use ieee.std_logic_1164.all;

entity qudq is
  port (nyepyieub : out std_logic_vector(3 to 1); anacjojq : in time_vector(0 downto 1); smuloouww : buffer std_logic; fcmx : inout std_logic);
end qudq;

architecture tmdqq of qudq is
  
begin
  -- Multi-driven assignments
  nyepyieub <= nyepyieub;
  fcmx <= 'U';
  fcmx <= 'L';
  smuloouww <= 'H';
end tmdqq;

entity dsexdii is
  port (vkkqtcm : out integer; sfolrdwqwc : out time);
end dsexdii;

library ieee;
use ieee.std_logic_1164.all;

architecture fauvos of dsexdii is
  signal yuhkgc : std_logic;
  signal yo : time_vector(0 downto 1);
  signal blmhtu : std_logic;
  signal rlpvo : time_vector(0 downto 1);
  signal illhrjq : std_logic;
  signal l : std_logic;
  signal bfrfog : time_vector(0 downto 1);
  signal shvhbpa : std_logic_vector(3 to 1);
begin
  vvqsxukty : entity work.qudq
    port map (nyepyieub => shvhbpa, anacjojq => bfrfog, smuloouww => l, fcmx => illhrjq);
  iznw : entity work.qudq
    port map (nyepyieub => shvhbpa, anacjojq => rlpvo, smuloouww => blmhtu, fcmx => l);
  euezmuot : entity work.qudq
    port map (nyepyieub => shvhbpa, anacjojq => yo, smuloouww => yuhkgc, fcmx => yuhkgc);
  
  -- Single-driven assignments
  bfrfog <= (others => 0 ns);
  vkkqtcm <= vkkqtcm;
  yo <= (others => 0 ns);
  sfolrdwqwc <= 12112 us;
  
  -- Multi-driven assignments
  shvhbpa <= shvhbpa;
  blmhtu <= '0';
end fauvos;

library ieee;
use ieee.std_logic_1164.all;

entity ngffexjbcp is
  port (wh : linkage time; ebi : buffer real; gwmpkxvljd : in std_logic; fii : linkage time);
end ngffexjbcp;

library ieee;
use ieee.std_logic_1164.all;

architecture ysgwjjqo of ngffexjbcp is
  signal ajtr : std_logic;
  signal bmlc : std_logic_vector(3 to 1);
  signal xmlrallund : std_logic;
  signal vaynrp : std_logic;
  signal nfrv : std_logic_vector(3 to 1);
  signal jdgyg : std_logic;
  signal qu : std_logic;
  signal ayigov : time_vector(0 downto 1);
  signal ndbjdol : std_logic_vector(3 to 1);
begin
  xs : entity work.qudq
    port map (nyepyieub => ndbjdol, anacjojq => ayigov, smuloouww => qu, fcmx => jdgyg);
  zk : entity work.qudq
    port map (nyepyieub => nfrv, anacjojq => ayigov, smuloouww => vaynrp, fcmx => xmlrallund);
  vfzoiqaaz : entity work.qudq
    port map (nyepyieub => bmlc, anacjojq => ayigov, smuloouww => qu, fcmx => ajtr);
  
  -- Single-driven assignments
  ebi <= ebi;
  ayigov <= ayigov;
  
  -- Multi-driven assignments
  bmlc <= (others => '0');
  xmlrallund <= 'Z';
  qu <= 'H';
end ysgwjjqo;

library ieee;
use ieee.std_logic_1164.all;

entity ah is
  port (gdsij : in real; hsjluutz : out time_vector(2 to 3); gcngpk : inout std_logic);
end ah;

library ieee;
use ieee.std_logic_1164.all;

architecture ufdmbrkogt of ah is
  signal vaykl : time;
  signal hp : std_logic;
  signal hib : real;
  signal mx : time;
  signal dibmxmh : std_logic;
  signal getzlhxr : std_logic;
  signal tf : std_logic;
  signal y : std_logic;
  signal yqj : time_vector(0 downto 1);
  signal jlpqlrkcy : std_logic_vector(3 to 1);
begin
  ltedls : entity work.qudq
    port map (nyepyieub => jlpqlrkcy, anacjojq => yqj, smuloouww => y, fcmx => tf);
  otzeipvfj : entity work.qudq
    port map (nyepyieub => jlpqlrkcy, anacjojq => yqj, smuloouww => getzlhxr, fcmx => dibmxmh);
  wbe : entity work.ngffexjbcp
    port map (wh => mx, ebi => hib, gwmpkxvljd => hp, fii => vaykl);
  
  -- Single-driven assignments
  yqj <= (others => 0 ns);
  hsjluutz <= hsjluutz;
end ufdmbrkogt;



-- Seed after: 13485821024697185573,2511821214772927453
