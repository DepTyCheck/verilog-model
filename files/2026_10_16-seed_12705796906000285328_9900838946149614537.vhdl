-- Seed: 12705796906000285328,9900838946149614537

library ieee;
use ieee.std_logic_1164.all;

entity nxgmjwrit is
  port (gjzxjpv : out real; yaehgkoe : out std_logic; gyofdohlmq : inout real);
end nxgmjwrit;

architecture onwr of nxgmjwrit is
  
begin
  -- Single-driven assignments
  gyofdohlmq <= 16#9_B_7_1_5.F6#;
  gjzxjpv <= gyofdohlmq;
  
  -- Multi-driven assignments
  yaehgkoe <= 'X';
end onwr;

library ieee;
use ieee.std_logic_1164.all;

entity djikr is
  port (noqak : buffer std_logic; mcwisnyk : linkage severity_level; b : in time);
end djikr;

library ieee;
use ieee.std_logic_1164.all;

architecture s of djikr is
  signal qgmwwhhuks : real;
  signal q : std_logic;
  signal csrjzhrcw : real;
  signal ziondaq : real;
  signal xpjbykggh : std_logic;
  signal d : real;
  signal idu : real;
  signal fbo : real;
  signal dp : real;
  signal xgkrcbng : std_logic;
  signal ixvqdj : real;
begin
  lkjdfpbpv : entity work.nxgmjwrit
    port map (gjzxjpv => ixvqdj, yaehgkoe => xgkrcbng, gyofdohlmq => dp);
  msbd : entity work.nxgmjwrit
    port map (gjzxjpv => fbo, yaehgkoe => noqak, gyofdohlmq => idu);
  ijvy : entity work.nxgmjwrit
    port map (gjzxjpv => d, yaehgkoe => xpjbykggh, gyofdohlmq => ziondaq);
  mti : entity work.nxgmjwrit
    port map (gjzxjpv => csrjzhrcw, yaehgkoe => q, gyofdohlmq => qgmwwhhuks);
  
  -- Multi-driven assignments
  xgkrcbng <= noqak;
end s;

entity xsxcn is
  port (nqzgoavdby : inout time; qbf : out integer; exfe : linkage real);
end xsxcn;

library ieee;
use ieee.std_logic_1164.all;

architecture wuxrsq of xsxcn is
  signal swcaul : time;
  signal l : severity_level;
  signal wjkml : std_logic;
begin
  dksulvr : entity work.djikr
    port map (noqak => wjkml, mcwisnyk => l, b => swcaul);
  
  -- Single-driven assignments
  swcaul <= nqzgoavdby;
  nqzgoavdby <= nqzgoavdby;
  qbf <= qbf;
  
  -- Multi-driven assignments
  wjkml <= 'H';
  wjkml <= wjkml;
  wjkml <= wjkml;
  wjkml <= wjkml;
end wuxrsq;

library ieee;
use ieee.std_logic_1164.all;

entity cyodbdamt is
  port (rwhh : buffer std_logic_vector(2 to 3); mekeej : inout integer; rtttjt : out std_logic; hjhkoenvm : buffer std_logic_vector(1 to 4));
end cyodbdamt;

architecture scudk of cyodbdamt is
  signal yp : time;
  signal efo : severity_level;
  signal dkmhtzrysv : real;
  signal vgudkrtf : real;
begin
  ruxfs : entity work.nxgmjwrit
    port map (gjzxjpv => vgudkrtf, yaehgkoe => rtttjt, gyofdohlmq => dkmhtzrysv);
  f : entity work.djikr
    port map (noqak => rtttjt, mcwisnyk => efo, b => yp);
  
  -- Single-driven assignments
  yp <= 4_1_2_4_0 fs;
  mekeej <= 343;
end scudk;



-- Seed after: 11886245892024992430,9900838946149614537
