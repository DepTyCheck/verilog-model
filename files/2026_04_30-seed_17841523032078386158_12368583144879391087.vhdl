-- Seed: 17841523032078386158,12368583144879391087

library ieee;
use ieee.std_logic_1164.all;

entity jsugrupvb is
  port (zzjbkyjezm : in std_logic; fngd : buffer real; vxwdl : in time; qfah : inout std_logic);
end jsugrupvb;



architecture axtjnwmfb of jsugrupvb is
  
begin
  
end axtjnwmfb;



entity p is
  port (jpbip : buffer boolean; izis : buffer time);
end p;

library ieee;
use ieee.std_logic_1164.all;

architecture wjw of p is
  signal slpzu : time;
  signal yjvmzxgbkx : real;
  signal hnrmgpwa : std_logic;
  signal am : std_logic;
  signal dor : real;
  signal nwltwsfxh : std_logic;
  signal ggb : time;
  signal tmeul : real;
  signal uhmhqtw : time;
  signal veqdcil : real;
  signal hw : std_logic;
begin
  swlx : entity work.jsugrupvb
    port map (zzjbkyjezm => hw, fngd => veqdcil, vxwdl => uhmhqtw, qfah => hw);
  ii : entity work.jsugrupvb
    port map (zzjbkyjezm => hw, fngd => tmeul, vxwdl => ggb, qfah => nwltwsfxh);
  kgljycd : entity work.jsugrupvb
    port map (zzjbkyjezm => nwltwsfxh, fngd => dor, vxwdl => izis, qfah => am);
  ltrteaxcy : entity work.jsugrupvb
    port map (zzjbkyjezm => hnrmgpwa, fngd => yjvmzxgbkx, vxwdl => slpzu, qfah => hw);
end wjw;



entity sfsuwmt is
  port (kvqkjidau : out real; i : out integer; zzsbwghfg : in real);
end sfsuwmt;

library ieee;
use ieee.std_logic_1164.all;

architecture ifjtmr of sfsuwmt is
  signal cpyw : real;
  signal v : time;
  signal woclktxvv : real;
  signal mcci : std_logic;
  signal ktbryckkol : time;
  signal ykqhrou : boolean;
begin
  qpi : entity work.p
    port map (jpbip => ykqhrou, izis => ktbryckkol);
  bsqkkrxsj : entity work.jsugrupvb
    port map (zzjbkyjezm => mcci, fngd => woclktxvv, vxwdl => v, qfah => mcci);
  qysrgr : entity work.jsugrupvb
    port map (zzjbkyjezm => mcci, fngd => cpyw, vxwdl => ktbryckkol, qfah => mcci);
end ifjtmr;

library ieee;
use ieee.std_logic_1164.all;

entity uzqgjtej is
  port (nxrnikhssr : buffer integer; hcmuvsg : buffer real; jwuodmzukt : linkage std_logic);
end uzqgjtej;

library ieee;
use ieee.std_logic_1164.all;

architecture ziwqtazo of uzqgjtej is
  signal hi : real;
  signal i : std_logic;
  signal coqnr : time;
  signal pcshwquc : boolean;
  signal gnw : real;
begin
  fq : entity work.sfsuwmt
    port map (kvqkjidau => gnw, i => nxrnikhssr, zzsbwghfg => gnw);
  sxnqus : entity work.p
    port map (jpbip => pcshwquc, izis => coqnr);
  rhrhdh : entity work.jsugrupvb
    port map (zzjbkyjezm => i, fngd => hi, vxwdl => coqnr, qfah => i);
end ziwqtazo;



-- Seed after: 4302141133160378369,12368583144879391087
