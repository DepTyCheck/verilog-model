-- Seed: 15710260854526854084,4404421571376382767

library ieee;
use ieee.std_logic_1164.all;

entity imv is
  port (ryarpvte : buffer integer; jplm : in std_logic);
end imv;

architecture cnyufzyxk of imv is
  
begin
  -- Single-driven assignments
  ryarpvte <= ryarpvte;
end cnyufzyxk;

library ieee;
use ieee.std_logic_1164.all;

entity yusztjz is
  port (qmeipa : in std_logic; xxafjajy : buffer time; jttgyzdxt : buffer bit_vector(0 downto 1));
end yusztjz;

library ieee;
use ieee.std_logic_1164.all;

architecture lsgzgxvucp of yusztjz is
  signal bksoymxm : std_logic;
  signal qy : integer;
  signal gi : std_logic;
  signal mqgcxr : integer;
begin
  tdwaa : entity work.imv
    port map (ryarpvte => mqgcxr, jplm => gi);
  oecxgf : entity work.imv
    port map (ryarpvte => qy, jplm => bksoymxm);
  
  -- Multi-driven assignments
  gi <= 'U';
  bksoymxm <= 'L';
  bksoymxm <= 'H';
  gi <= '0';
end lsgzgxvucp;

library ieee;
use ieee.std_logic_1164.all;

entity tq is
  port (awzjxt : buffer bit_vector(3 to 0); ybftjstxan : inout std_logic);
end tq;

architecture slugq of tq is
  signal abdugf : integer;
  signal k : time;
begin
  ebcjnhamud : entity work.yusztjz
    port map (qmeipa => ybftjstxan, xxafjajy => k, jttgyzdxt => awzjxt);
  qsmlrpfat : entity work.imv
    port map (ryarpvte => abdugf, jplm => ybftjstxan);
  
  -- Multi-driven assignments
  ybftjstxan <= ybftjstxan;
  ybftjstxan <= 'U';
end slugq;

entity ssxbskt is
  port (wkqgco : buffer time);
end ssxbskt;

library ieee;
use ieee.std_logic_1164.all;

architecture dxvpqruq of ssxbskt is
  signal dlunw : std_logic;
  signal kctuaajpc : bit_vector(3 to 0);
  signal obiqdazb : integer;
  signal fcgzab : std_logic;
  signal nrouffkxez : integer;
begin
  nf : entity work.imv
    port map (ryarpvte => nrouffkxez, jplm => fcgzab);
  ccf : entity work.imv
    port map (ryarpvte => obiqdazb, jplm => fcgzab);
  ljkoq : entity work.tq
    port map (awzjxt => kctuaajpc, ybftjstxan => dlunw);
  
  -- Single-driven assignments
  wkqgco <= 3 sec;
  
  -- Multi-driven assignments
  fcgzab <= 'L';
  dlunw <= fcgzab;
  dlunw <= fcgzab;
  dlunw <= 'L';
end dxvpqruq;



-- Seed after: 12099900067826865238,4404421571376382767
