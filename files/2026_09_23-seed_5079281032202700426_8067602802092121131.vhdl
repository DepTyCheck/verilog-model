-- Seed: 5079281032202700426,8067602802092121131

library ieee;
use ieee.std_logic_1164.all;

entity arq is
  port (uwhkkbe : out time; zdp : in std_logic; ksxgrxad : in boolean; k : linkage real);
end arq;

architecture xzyp of arq is
  
begin
  -- Single-driven assignments
  uwhkkbe <= 0.2_1_0_2 us;
end xzyp;

library ieee;
use ieee.std_logic_1164.all;

entity aunsexk is
  port (qratsey : inout std_logic; kdtnq : buffer real; qcpevbm : buffer time_vector(4 downto 2));
end aunsexk;

library ieee;
use ieee.std_logic_1164.all;

architecture s of aunsexk is
  signal qkkpxbsig : real;
  signal pgbi : std_logic;
  signal iezqhp : time;
  signal wujfit : boolean;
  signal jwgguxikln : time;
  signal szsad : real;
  signal gllzzc : boolean;
  signal nfnebiuv : std_logic;
  signal sdbdval : time;
begin
  wedwx : entity work.arq
    port map (uwhkkbe => sdbdval, zdp => nfnebiuv, ksxgrxad => gllzzc, k => szsad);
  il : entity work.arq
    port map (uwhkkbe => jwgguxikln, zdp => qratsey, ksxgrxad => wujfit, k => kdtnq);
  talblzdq : entity work.arq
    port map (uwhkkbe => iezqhp, zdp => pgbi, ksxgrxad => wujfit, k => qkkpxbsig);
  
  -- Multi-driven assignments
  pgbi <= qratsey;
  qratsey <= '1';
end s;

library ieee;
use ieee.std_logic_1164.all;

entity pxle is
  port (qj : linkage std_logic_vector(4 downto 2); mscwsepp : linkage real);
end pxle;

library ieee;
use ieee.std_logic_1164.all;

architecture bjfuopqgyk of pxle is
  signal dsjwezi : boolean;
  signal uckrcjvk : time;
  signal bdkrxn : time_vector(4 downto 2);
  signal tmhdt : real;
  signal qhjk : time_vector(4 downto 2);
  signal p : real;
  signal zzbxdt : real;
  signal wyvoobnq : boolean;
  signal v : std_logic;
  signal hwxqdqm : time;
begin
  ga : entity work.arq
    port map (uwhkkbe => hwxqdqm, zdp => v, ksxgrxad => wyvoobnq, k => zzbxdt);
  vher : entity work.aunsexk
    port map (qratsey => v, kdtnq => p, qcpevbm => qhjk);
  wi : entity work.aunsexk
    port map (qratsey => v, kdtnq => tmhdt, qcpevbm => bdkrxn);
  svxcwj : entity work.arq
    port map (uwhkkbe => uckrcjvk, zdp => v, ksxgrxad => dsjwezi, k => mscwsepp);
  
  -- Single-driven assignments
  wyvoobnq <= FALSE;
  dsjwezi <= wyvoobnq;
  
  -- Multi-driven assignments
  v <= 'H';
  v <= 'Z';
  v <= v;
  v <= v;
end bjfuopqgyk;



-- Seed after: 12881994741871417958,8067602802092121131
