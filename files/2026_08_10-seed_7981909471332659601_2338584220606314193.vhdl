-- Seed: 7981909471332659601,2338584220606314193

library ieee;
use ieee.std_logic_1164.all;

entity mg is
  port (cdqjlldj : buffer boolean; kzwdtpol : buffer real_vector(1 to 1); jrbummrtr : inout std_logic);
end mg;

architecture goopy of mg is
  
begin
  -- Single-driven assignments
  kzwdtpol <= kzwdtpol;
  cdqjlldj <= cdqjlldj;
  
  -- Multi-driven assignments
  jrbummrtr <= 'L';
  jrbummrtr <= 'U';
  jrbummrtr <= jrbummrtr;
end goopy;

library ieee;
use ieee.std_logic_1164.all;

entity fmhr is
  port (bzbjbj : out time; dy : buffer std_logic_vector(0 downto 4));
end fmhr;

library ieee;
use ieee.std_logic_1164.all;

architecture yqlptfezbk of fmhr is
  signal eodxus : std_logic;
  signal xx : real_vector(1 to 1);
  signal jxsfaeo : boolean;
begin
  y : entity work.mg
    port map (cdqjlldj => jxsfaeo, kzwdtpol => xx, jrbummrtr => eodxus);
end yqlptfezbk;

entity bqigjujuza is
  port (gz : in integer);
end bqigjujuza;

library ieee;
use ieee.std_logic_1164.all;

architecture l of bqigjujuza is
  signal y : std_logic_vector(0 downto 4);
  signal ywtspoyl : time;
  signal cnspyr : real_vector(1 to 1);
  signal monjrmrl : boolean;
  signal ogb : real_vector(1 to 1);
  signal cckbpwnwpk : boolean;
  signal nddm : std_logic;
  signal hdlvexg : real_vector(1 to 1);
  signal bhsbsomd : boolean;
begin
  doxl : entity work.mg
    port map (cdqjlldj => bhsbsomd, kzwdtpol => hdlvexg, jrbummrtr => nddm);
  gjsymynqvi : entity work.mg
    port map (cdqjlldj => cckbpwnwpk, kzwdtpol => ogb, jrbummrtr => nddm);
  yoelzliym : entity work.mg
    port map (cdqjlldj => monjrmrl, kzwdtpol => cnspyr, jrbummrtr => nddm);
  ecsbzfzb : entity work.fmhr
    port map (bzbjbj => ywtspoyl, dy => y);
  
  -- Multi-driven assignments
  nddm <= '0';
end l;



-- Seed after: 16313504413647465608,2338584220606314193
