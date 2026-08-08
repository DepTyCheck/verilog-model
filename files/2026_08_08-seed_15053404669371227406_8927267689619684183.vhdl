-- Seed: 15053404669371227406,8927267689619684183

library ieee;
use ieee.std_logic_1164.all;

entity vvsxqd is
  port (hzzzisr : inout real; paayoxkbyw : inout std_logic);
end vvsxqd;

architecture qjqucc of vvsxqd is
  
begin
  -- Single-driven assignments
  hzzzisr <= 31.132;
  
  -- Multi-driven assignments
  paayoxkbyw <= 'U';
  paayoxkbyw <= 'X';
  paayoxkbyw <= 'Z';
  paayoxkbyw <= 'W';
end qjqucc;

library ieee;
use ieee.std_logic_1164.all;

entity g is
  port (mdb : out std_logic; nanraz : in std_logic; ar : out time);
end g;

library ieee;
use ieee.std_logic_1164.all;

architecture il of g is
  signal h : real;
  signal dt : real;
  signal gh : std_logic;
  signal qmzyeuax : real;
begin
  zfwdnqdi : entity work.vvsxqd
    port map (hzzzisr => qmzyeuax, paayoxkbyw => gh);
  pon : entity work.vvsxqd
    port map (hzzzisr => dt, paayoxkbyw => gh);
  sws : entity work.vvsxqd
    port map (hzzzisr => h, paayoxkbyw => mdb);
  
  -- Single-driven assignments
  ar <= 8#1021# ns;
  
  -- Multi-driven assignments
  mdb <= 'Z';
  mdb <= 'X';
  mdb <= mdb;
  mdb <= '-';
end il;



-- Seed after: 7516259340427212219,8927267689619684183
