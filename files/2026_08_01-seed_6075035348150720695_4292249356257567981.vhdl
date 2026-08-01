-- Seed: 6075035348150720695,4292249356257567981

library ieee;
use ieee.std_logic_1164.all;

entity aqxhxqgkn is
  port (jt : inout std_logic; etwlba : buffer boolean; hszlkx : out time);
end aqxhxqgkn;

architecture wm of aqxhxqgkn is
  
begin
  -- Multi-driven assignments
  jt <= '1';
end wm;

library ieee;
use ieee.std_logic_1164.all;

entity yh is
  port (gvrqf : linkage time_vector(2 downto 0); bms : out bit; iiynn : buffer integer; gtxpskzs : out std_logic);
end yh;

library ieee;
use ieee.std_logic_1164.all;

architecture ywgujlte of yh is
  signal pgg : time;
  signal ccmco : boolean;
  signal eicxwidrj : time;
  signal zgwdj : boolean;
  signal jlw : std_logic;
begin
  qgil : entity work.aqxhxqgkn
    port map (jt => jlw, etwlba => zgwdj, hszlkx => eicxwidrj);
  xylcos : entity work.aqxhxqgkn
    port map (jt => gtxpskzs, etwlba => ccmco, hszlkx => pgg);
end ywgujlte;

library ieee;
use ieee.std_logic_1164.all;

entity whlouiv is
  port (xm : linkage std_logic_vector(1 to 1); zgkkvhchb : inout real_vector(3 to 0); hzd : linkage severity_level; sfvoydnp : in integer);
end whlouiv;

library ieee;
use ieee.std_logic_1164.all;

architecture wb of whlouiv is
  signal yt : std_logic;
  signal uvndz : integer;
  signal isbz : bit;
  signal nwrsoqo : time_vector(2 downto 0);
  signal btt : time;
  signal cmzhnygu : boolean;
  signal auigdk : std_logic;
  signal sbb : time;
  signal rbqp : boolean;
  signal eqxhqcwpnm : std_logic;
begin
  lbgq : entity work.aqxhxqgkn
    port map (jt => eqxhqcwpnm, etwlba => rbqp, hszlkx => sbb);
  cezfaag : entity work.aqxhxqgkn
    port map (jt => auigdk, etwlba => cmzhnygu, hszlkx => btt);
  mxadjgour : entity work.yh
    port map (gvrqf => nwrsoqo, bms => isbz, iiynn => uvndz, gtxpskzs => yt);
  
  -- Single-driven assignments
  zgkkvhchb <= zgkkvhchb;
  
  -- Multi-driven assignments
  eqxhqcwpnm <= eqxhqcwpnm;
  eqxhqcwpnm <= eqxhqcwpnm;
  eqxhqcwpnm <= '0';
end wb;



-- Seed after: 16368294380376159321,4292249356257567981
