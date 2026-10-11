-- Seed: 2919292941226503333,9099465110516446487

entity g is
  port (snjlimmu : inout real; wqmxw : buffer integer; aycbaasn : linkage real; frr : inout integer);
end g;

architecture tbh of g is
  
begin
  -- Single-driven assignments
  frr <= wqmxw;
  snjlimmu <= snjlimmu;
  wqmxw <= frr;
end tbh;

library ieee;
use ieee.std_logic_1164.all;

entity zg is
  port (kvqqvidec : buffer std_logic);
end zg;

architecture ttxem of zg is
  signal ci : integer;
  signal rt : real;
  signal ou : integer;
  signal hm : real;
  signal mojweeobz : integer;
  signal xdg : real;
  signal omcq : integer;
  signal vgu : real;
  signal gicwlesiu : integer;
  signal oiilpieqp : real;
  signal phbpuol : integer;
  signal v : real;
begin
  pabmom : entity work.g
    port map (snjlimmu => v, wqmxw => phbpuol, aycbaasn => oiilpieqp, frr => gicwlesiu);
  enu : entity work.g
    port map (snjlimmu => vgu, wqmxw => omcq, aycbaasn => xdg, frr => mojweeobz);
  m : entity work.g
    port map (snjlimmu => hm, wqmxw => ou, aycbaasn => rt, frr => ci);
  
  -- Multi-driven assignments
  kvqqvidec <= kvqqvidec;
end ttxem;

entity mvlozqpy is
  port (rpba : out boolean);
end mvlozqpy;

architecture cnmtz of mvlozqpy is
  signal kyrfcgehc : integer;
  signal u : real;
  signal crmzba : integer;
  signal qnje : real;
  signal xejtuwodhn : integer;
  signal wc : real;
  signal kxnhg : integer;
  signal memairzsth : real;
  signal wzijtigzxt : integer;
  signal fgsexsi : real;
  signal csjzat : integer;
  signal wjhwfx : real;
begin
  lghihbytp : entity work.g
    port map (snjlimmu => wjhwfx, wqmxw => csjzat, aycbaasn => fgsexsi, frr => wzijtigzxt);
  nyaawly : entity work.g
    port map (snjlimmu => memairzsth, wqmxw => kxnhg, aycbaasn => wc, frr => xejtuwodhn);
  kwsatbot : entity work.g
    port map (snjlimmu => qnje, wqmxw => crmzba, aycbaasn => u, frr => kyrfcgehc);
end cnmtz;

library ieee;
use ieee.std_logic_1164.all;

entity tyy is
  port (k : in std_logic; p : inout time; rw : buffer real; xfunmmigc : buffer boolean);
end tyy;

architecture wfzieoqari of tyy is
  signal vlv : boolean;
  signal biqp : integer;
  signal ui : real;
  signal rbdoytbx : integer;
  signal cwergax : real;
  signal iace : integer;
  signal ksclqd : integer;
  signal iffo : real;
begin
  t : entity work.g
    port map (snjlimmu => iffo, wqmxw => ksclqd, aycbaasn => rw, frr => iace);
  c : entity work.g
    port map (snjlimmu => cwergax, wqmxw => rbdoytbx, aycbaasn => ui, frr => biqp);
  kgaczt : entity work.mvlozqpy
    port map (rpba => vlv);
  tkjb : entity work.mvlozqpy
    port map (rpba => xfunmmigc);
  
  -- Single-driven assignments
  p <= 33231 fs;
end wfzieoqari;



-- Seed after: 16503631878612430376,9099465110516446487
