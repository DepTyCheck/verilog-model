-- Seed: 4728171870431970811,3820899062418988741



entity lrtwppav is
  port (kzvz : linkage integer; phxw : inout severity_level; dsuhyoeetm : linkage real);
end lrtwppav;



architecture gaseqq of lrtwppav is
  
begin
  
end gaseqq;



entity hkb is
  port (nvktsouty : out integer);
end hkb;



architecture speim of hkb is
  signal dwhh : real;
  signal xkbxdte : severity_level;
begin
  hm : entity work.lrtwppav
    port map (kzvz => nvktsouty, phxw => xkbxdte, dsuhyoeetm => dwhh);
end speim;

library ieee;
use ieee.std_logic_1164.all;

entity dwzbyc is
  port (hwcury : buffer std_logic);
end dwzbyc;



architecture zw of dwzbyc is
  signal fpridhxnbp : real;
  signal iuhmse : severity_level;
  signal mfvzhg : real;
  signal zhffubpm : severity_level;
  signal vlnwk : integer;
  signal dblmhwsbt : real;
  signal kjghjcy : severity_level;
  signal kiitgjwqlh : integer;
begin
  wqmihugjjg : entity work.lrtwppav
    port map (kzvz => kiitgjwqlh, phxw => kjghjcy, dsuhyoeetm => dblmhwsbt);
  oksrxb : entity work.lrtwppav
    port map (kzvz => vlnwk, phxw => zhffubpm, dsuhyoeetm => mfvzhg);
  c : entity work.lrtwppav
    port map (kzvz => kiitgjwqlh, phxw => iuhmse, dsuhyoeetm => fpridhxnbp);
end zw;

library ieee;
use ieee.std_logic_1164.all;

entity sa is
  port (awpigssuwd : inout severity_level; udrhv : out real; y : buffer std_logic);
end sa;



architecture paegl of sa is
  signal i : real;
  signal vhnhjhd : integer;
begin
  xwrjl : entity work.dwzbyc
    port map (hwcury => y);
  gwaacxeawg : entity work.lrtwppav
    port map (kzvz => vhnhjhd, phxw => awpigssuwd, dsuhyoeetm => i);
  uxv : entity work.hkb
    port map (nvktsouty => vhnhjhd);
end paegl;



-- Seed after: 8246724323580652440,3820899062418988741
