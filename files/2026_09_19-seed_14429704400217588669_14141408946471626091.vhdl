-- Seed: 14429704400217588669,14141408946471626091

library ieee;
use ieee.std_logic_1164.all;

entity wji is
  port (qjrgtm : out std_logic_vector(1 to 1); nahpcdh : linkage boolean; i : inout std_logic; wtjvfro : in integer);
end wji;

architecture lgs of wji is
  
begin
  -- Multi-driven assignments
  i <= i;
  i <= 'X';
  i <= i;
end lgs;

entity qwkiskbe is
  port (zmsgjb : inout integer; wsxiey : linkage severity_level);
end qwkiskbe;

library ieee;
use ieee.std_logic_1164.all;

architecture nf of qwkiskbe is
  signal etxjgpb : std_logic;
  signal ptyubaftmr : boolean;
  signal olcbkuqz : std_logic_vector(1 to 1);
  signal oiajfl : std_logic;
  signal gcnamw : boolean;
  signal xvlu : std_logic_vector(1 to 1);
  signal vgzdk : integer;
  signal ciycjoyve : std_logic;
  signal i : boolean;
  signal sagmtx : std_logic_vector(1 to 1);
begin
  xlcesktbdr : entity work.wji
    port map (qjrgtm => sagmtx, nahpcdh => i, i => ciycjoyve, wtjvfro => vgzdk);
  ukrxlyx : entity work.wji
    port map (qjrgtm => xvlu, nahpcdh => gcnamw, i => oiajfl, wtjvfro => zmsgjb);
  x : entity work.wji
    port map (qjrgtm => olcbkuqz, nahpcdh => ptyubaftmr, i => etxjgpb, wtjvfro => zmsgjb);
  
  -- Single-driven assignments
  vgzdk <= zmsgjb;
  zmsgjb <= 34000;
  
  -- Multi-driven assignments
  oiajfl <= ciycjoyve;
  xvlu <= sagmtx;
  oiajfl <= 'U';
end nf;



-- Seed after: 8512486478737597174,14141408946471626091
