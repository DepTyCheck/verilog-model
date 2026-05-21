-- Seed: 11426839345742970427,11460338385561364835



entity fmmps is
  port (i : in time; ibndwzabf : linkage real; imv : inout integer; yeyo : inout time);
end fmmps;



architecture jhqlqwqox of fmmps is
  
begin
  
end jhqlqwqox;

library ieee;
use ieee.std_logic_1164.all;

entity u is
  port (vzahtjokzo : linkage std_logic; dlzxzf : inout integer; tq : inout std_logic);
end u;



architecture b of u is
  signal jnigtvy : time;
  signal q : real;
  signal xkvxyoeutr : time;
  signal phlpck : time;
  signal fyi : integer;
  signal acqyzmadl : real;
  signal xzctvhvd : integer;
  signal gilznevyg : real;
  signal dyk : time;
begin
  vdz : entity work.fmmps
    port map (i => dyk, ibndwzabf => gilznevyg, imv => xzctvhvd, yeyo => dyk);
  vxidzqf : entity work.fmmps
    port map (i => dyk, ibndwzabf => acqyzmadl, imv => fyi, yeyo => phlpck);
  mslhvxejv : entity work.fmmps
    port map (i => xkvxyoeutr, ibndwzabf => q, imv => dlzxzf, yeyo => jnigtvy);
end b;



entity euyi is
  port (ppaxgmtu : in integer; fz : linkage real);
end euyi;

library ieee;
use ieee.std_logic_1164.all;

architecture wvtr of euyi is
  signal loquwpaxn : std_logic;
  signal edto : integer;
  signal p : std_logic;
begin
  zqvsnludw : entity work.u
    port map (vzahtjokzo => p, dlzxzf => edto, tq => loquwpaxn);
end wvtr;



entity qncxpirhs is
  port (kzkbgflz : out bit; lxxytojrwg : in time);
end qncxpirhs;



architecture n of qncxpirhs is
  signal a : real;
  signal ctharcot : time;
  signal xy : real;
  signal ptmgm : integer;
  signal rintw : integer;
  signal uilgbtmbp : real;
  signal ssr : time;
begin
  t : entity work.fmmps
    port map (i => ssr, ibndwzabf => uilgbtmbp, imv => rintw, yeyo => ssr);
  dxevvn : entity work.euyi
    port map (ppaxgmtu => ptmgm, fz => xy);
  p : entity work.fmmps
    port map (i => ctharcot, ibndwzabf => a, imv => ptmgm, yeyo => ctharcot);
end n;



-- Seed after: 742498388685384751,11460338385561364835
