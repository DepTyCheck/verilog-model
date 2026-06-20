-- Seed: 14700967792022278438,3924983747739634027

entity leembci is
  port (gckkj : inout time);
end leembci;

architecture b of leembci is
  
begin
  -- Single-driven assignments
  gckkj <= 0 min;
end b;

library ieee;
use ieee.std_logic_1164.all;

entity upx is
  port (iyh : linkage std_logic_vector(4 to 3); ztorudglof : out time; vgrpguwktk : out time_vector(2 to 3); ualavx : inout std_logic);
end upx;

architecture rpolshomuk of upx is
  
begin
  hgdbistt : entity work.leembci
    port map (gckkj => ztorudglof);
  
  -- Single-driven assignments
  vgrpguwktk <= (8#5540.030# ms, 4 sec);
end rpolshomuk;

entity saszade is
  port (zzhnwlki : linkage time; czufu : in time; hyelzesmqh : out real_vector(1 to 2));
end saszade;

library ieee;
use ieee.std_logic_1164.all;

architecture zzm of saszade is
  signal lxrrhz : time;
  signal vvqgp : time;
  signal cvnqqlcfa : time;
  signal t : std_logic;
  signal jktp : time_vector(2 to 3);
  signal u : time;
  signal beu : std_logic_vector(4 to 3);
begin
  ric : entity work.upx
    port map (iyh => beu, ztorudglof => u, vgrpguwktk => jktp, ualavx => t);
  osjw : entity work.leembci
    port map (gckkj => cvnqqlcfa);
  lcnkbinrb : entity work.leembci
    port map (gckkj => vvqgp);
  xstrjmytlg : entity work.leembci
    port map (gckkj => lxrrhz);
  
  -- Single-driven assignments
  hyelzesmqh <= (1433.0, 8#3_7_4_6.4#);
  
  -- Multi-driven assignments
  beu <= "";
  beu <= "";
end zzm;

entity nooywtpu is
  port (reqnkpzyww : linkage integer);
end nooywtpu;

architecture rahcnr of nooywtpu is
  signal zvvydltjb : time;
  signal w : time;
begin
  vwmjypwt : entity work.leembci
    port map (gckkj => w);
  cnvi : entity work.leembci
    port map (gckkj => zvvydltjb);
end rahcnr;



-- Seed after: 2013967266218800279,3924983747739634027
