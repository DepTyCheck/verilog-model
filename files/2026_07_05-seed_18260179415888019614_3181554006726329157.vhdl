-- Seed: 18260179415888019614,3181554006726329157

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity p is
  port (lgrsxvq : inout access_value_mirror; gllk : buffer std_logic; qbjdqzob : inout integer_value_mirror; gpqobgbzy : inout access_value_mirror);
end p;

architecture vdtbnpivlh of p is
  
begin
  -- Multi-driven assignments
  gllk <= 'W';
  gllk <= gllk;
end vdtbnpivlh;

use std.reflection.all;

entity zwiskaong is
  port (mwrkana : inout integer_value_mirror);
end zwiskaong;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture vfsv of zwiskaong is
  shared variable ts : access_value_mirror;
  shared variable mht : access_value_mirror;
  shared variable akja : access_value_mirror;
  shared variable ppb : integer_value_mirror;
  signal iptdpclmq : std_logic;
  shared variable qc : access_value_mirror;
begin
  j : entity work.p
    port map (lgrsxvq => qc, gllk => iptdpclmq, qbjdqzob => ppb, gpqobgbzy => akja);
  xfln : entity work.p
    port map (lgrsxvq => mht, gllk => iptdpclmq, qbjdqzob => mwrkana, gpqobgbzy => ts);
  
  -- Multi-driven assignments
  iptdpclmq <= iptdpclmq;
  iptdpclmq <= iptdpclmq;
end vfsv;



-- Seed after: 18166198797891976418,3181554006726329157
