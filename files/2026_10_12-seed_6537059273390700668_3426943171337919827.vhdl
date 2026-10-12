-- Seed: 6537059273390700668,3426943171337919827

library ieee;
use ieee.std_logic_1164.all;

entity h is
  port (gvduybzl : linkage std_logic_vector(4 to 1); uxzdbsfwcx : inout real; kzz : in integer; lnvqofsioi : in time);
end h;

architecture koilu of h is
  
begin
  -- Single-driven assignments
  uxzdbsfwcx <= uxzdbsfwcx;
end koilu;

entity shczwlrmo is
  port (trditqxx : out character; ovhotcwbxg : out boolean; qstngmjpcs : out time; pwmsd : in integer);
end shczwlrmo;

library ieee;
use ieee.std_logic_1164.all;

architecture vqvjrzlir of shczwlrmo is
  signal cw : real;
  signal utkyyoxs : std_logic_vector(4 to 1);
  signal ng : time;
  signal aqdagl : real;
  signal wxwoql : std_logic_vector(4 to 1);
begin
  yndeouox : entity work.h
    port map (gvduybzl => wxwoql, uxzdbsfwcx => aqdagl, kzz => pwmsd, lnvqofsioi => ng);
  xeugkqku : entity work.h
    port map (gvduybzl => utkyyoxs, uxzdbsfwcx => cw, kzz => pwmsd, lnvqofsioi => qstngmjpcs);
  
  -- Single-driven assignments
  ng <= qstngmjpcs;
  trditqxx <= 'z';
  
  -- Multi-driven assignments
  wxwoql <= "";
  wxwoql <= (others => '0');
  wxwoql <= "";
end vqvjrzlir;

library ieee;
use ieee.std_logic_1164.all;

entity ns is
  port (fdn : buffer std_logic; dcodtjavi : inout integer);
end ns;

library ieee;
use ieee.std_logic_1164.all;

architecture vrz of ns is
  signal xzmhqkpfj : time;
  signal lzvsjwhgtb : real;
  signal qmppqm : std_logic_vector(4 to 1);
  signal xjzkvvifvy : integer;
  signal gbauaeakko : boolean;
  signal kaz : character;
  signal ifkxvb : time;
  signal jpkpefqeu : real;
  signal soeip : std_logic_vector(4 to 1);
begin
  iwv : entity work.h
    port map (gvduybzl => soeip, uxzdbsfwcx => jpkpefqeu, kzz => dcodtjavi, lnvqofsioi => ifkxvb);
  falryobsl : entity work.shczwlrmo
    port map (trditqxx => kaz, ovhotcwbxg => gbauaeakko, qstngmjpcs => ifkxvb, pwmsd => xjzkvvifvy);
  oiajk : entity work.h
    port map (gvduybzl => qmppqm, uxzdbsfwcx => lzvsjwhgtb, kzz => dcodtjavi, lnvqofsioi => xzmhqkpfj);
  
  -- Single-driven assignments
  xjzkvvifvy <= dcodtjavi;
  dcodtjavi <= 2#0_0_1_0#;
  
  -- Multi-driven assignments
  fdn <= '0';
end vrz;



-- Seed after: 2661708782807823700,3426943171337919827
