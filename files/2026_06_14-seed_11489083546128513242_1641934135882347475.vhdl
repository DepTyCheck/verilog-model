-- Seed: 11489083546128513242,1641934135882347475

library ieee;
use ieee.std_logic_1164.all;

entity n is
  port (ufisdhsru : out std_logic);
end n;



architecture pejyiygul of n is
  
begin
  
end pejyiygul;

library ieee;
use ieee.std_logic_1164.all;

entity mzmu is
  port (w : buffer std_logic);
end mzmu;

library ieee;
use ieee.std_logic_1164.all;

architecture jidki of mzmu is
  signal jv : std_logic;
  signal kevoc : std_logic;
  signal yxradgc : std_logic;
begin
  rpnqjtehzr : entity work.n
    port map (ufisdhsru => yxradgc);
  b : entity work.n
    port map (ufisdhsru => kevoc);
  e : entity work.n
    port map (ufisdhsru => jv);
  cq : entity work.n
    port map (ufisdhsru => w);
end jidki;



entity clyq is
  port (f : out integer; irbj : buffer boolean; djpa : linkage integer);
end clyq;

library ieee;
use ieee.std_logic_1164.all;

architecture z of clyq is
  signal bypxrntoje : std_logic;
  signal dl : std_logic;
begin
  zbwwaza : entity work.mzmu
    port map (w => dl);
  xixpipabr : entity work.n
    port map (ufisdhsru => dl);
  xxyyyfbps : entity work.n
    port map (ufisdhsru => bypxrntoje);
end z;



entity tieyrxfgcp is
  port (oyslqib : out integer_vector(4 to 4));
end tieyrxfgcp;

library ieee;
use ieee.std_logic_1164.all;

architecture ivlszmtdu of tieyrxfgcp is
  signal qiub : std_logic;
  signal aslkxojes : std_logic;
  signal absrfv : integer;
  signal vgacvi : boolean;
  signal sp : integer;
begin
  t : entity work.clyq
    port map (f => sp, irbj => vgacvi, djpa => absrfv);
  vitzxj : entity work.mzmu
    port map (w => aslkxojes);
  rn : entity work.n
    port map (ufisdhsru => qiub);
end ivlszmtdu;



-- Seed after: 8107060171002907428,1641934135882347475
