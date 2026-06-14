-- Seed: 14574218459166340919,6298042963991371649

library ieee;
use ieee.std_logic_1164.all;

entity poxswhzla is
  port (atik : inout std_logic);
end poxswhzla;



architecture fy of poxswhzla is
  
begin
  
end fy;



entity wzpr is
  port (rxwb : buffer real_vector(4 to 4); h : out integer);
end wzpr;

library ieee;
use ieee.std_logic_1164.all;

architecture qyonozf of wzpr is
  signal livhsdo : std_logic;
  signal xogauk : std_logic;
begin
  ynmbwfw : entity work.poxswhzla
    port map (atik => xogauk);
  ehzoulqo : entity work.poxswhzla
    port map (atik => xogauk);
  fcmt : entity work.poxswhzla
    port map (atik => livhsdo);
end qyonozf;

library ieee;
use ieee.std_logic_1164.all;

entity cab is
  port (rkpccruyo : inout std_logic_vector(4 to 1); bfhrts : buffer integer; vzi : inout std_logic);
end cab;



architecture egyw of cab is
  signal oyjezqrl : integer;
  signal vu : real_vector(4 to 4);
begin
  efuzsg : entity work.wzpr
    port map (rxwb => vu, h => oyjezqrl);
  yiof : entity work.poxswhzla
    port map (atik => vzi);
end egyw;



-- Seed after: 11763985498407312840,6298042963991371649
