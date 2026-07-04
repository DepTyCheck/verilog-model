-- Seed: 703971635453838303,6290177331721581829

entity tthd is
  port (a : linkage real; wsngimlfpq : buffer real);
end tthd;

architecture wvvkhczndu of tthd is
  
begin
  -- Single-driven assignments
  wsngimlfpq <= wsngimlfpq;
end wvvkhczndu;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity nxcwsroeh is
  port (qqnkhlg : out integer; twb : buffer std_logic; perdxjwlm : buffer boolean; sb : inout record_subtype_mirror);
end nxcwsroeh;

architecture l of nxcwsroeh is
  signal hsq : real;
  signal ps : real;
  signal utkr : real;
  signal sctckej : real;
  signal zhaoodsfq : real;
  signal qnje : real;
  signal ibuqiyokxd : real;
  signal gyhmogb : real;
begin
  izmnqkwifk : entity work.tthd
    port map (a => gyhmogb, wsngimlfpq => ibuqiyokxd);
  mbptgzunc : entity work.tthd
    port map (a => qnje, wsngimlfpq => zhaoodsfq);
  blcwwhvkvq : entity work.tthd
    port map (a => sctckej, wsngimlfpq => utkr);
  sq : entity work.tthd
    port map (a => ps, wsngimlfpq => hsq);
  
  -- Single-driven assignments
  qqnkhlg <= 0_0;
  perdxjwlm <= FALSE;
end l;

entity hvatzadk is
  port (wlccgouxp : out integer);
end hvatzadk;

architecture ptutznkb of hvatzadk is
  signal idbrbtwsv : real;
  signal njlntyhty : real;
  signal kis : real;
  signal poqswmojgg : real;
begin
  dctelzmnz : entity work.tthd
    port map (a => poqswmojgg, wsngimlfpq => kis);
  xunokjia : entity work.tthd
    port map (a => njlntyhty, wsngimlfpq => idbrbtwsv);
  
  -- Single-driven assignments
  wlccgouxp <= 8#6010#;
end ptutznkb;

use std.reflection.all;

entity k is
  port (ibq : inout access_value_mirror; akwoylnqhn : inout record_value_mirror; ogcw : inout integer_subtype_mirror);
end k;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture hqkpwo of k is
  signal solkewychn : real;
  signal ypet : real;
  shared variable awjvynw : record_subtype_mirror;
  signal ulcgtdmgjg : boolean;
  signal xfaayssrk : std_logic;
  signal uti : integer;
  signal cxbcawxfi : real;
  signal piks : real;
  signal coag : integer;
begin
  ndgzlvfrxs : entity work.hvatzadk
    port map (wlccgouxp => coag);
  exb : entity work.tthd
    port map (a => piks, wsngimlfpq => cxbcawxfi);
  yop : entity work.nxcwsroeh
    port map (qqnkhlg => uti, twb => xfaayssrk, perdxjwlm => ulcgtdmgjg, sb => awjvynw);
  bqg : entity work.tthd
    port map (a => ypet, wsngimlfpq => solkewychn);
  
  -- Multi-driven assignments
  xfaayssrk <= 'X';
  xfaayssrk <= xfaayssrk;
  xfaayssrk <= xfaayssrk;
  xfaayssrk <= 'U';
end hqkpwo;



-- Seed after: 551285736385678064,6290177331721581829
