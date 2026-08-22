-- Seed: 16236639980774276940,5805648483995786113

entity ruvty is
  port (tu : in time; dh : inout real);
end ruvty;

architecture ebwang of ruvty is
  
begin
  -- Single-driven assignments
  dh <= 1_1.3_4;
end ebwang;

library ieee;
use ieee.std_logic_1164.all;

entity nrz is
  port (yukrtabq : linkage std_logic_vector(1 downto 4); deilahlx : inout std_logic; lyp : in std_logic_vector(3 downto 0); lkgclxony : out real);
end nrz;

architecture ythh of nrz is
  signal dj : time;
  signal trb : real;
  signal jm : real;
  signal q : time;
  signal jherspila : real;
  signal gacgky : time;
begin
  jdfibc : entity work.ruvty
    port map (tu => gacgky, dh => jherspila);
  iojpdtgt : entity work.ruvty
    port map (tu => q, dh => jm);
  wkbpss : entity work.ruvty
    port map (tu => q, dh => trb);
  dcffdnolfc : entity work.ruvty
    port map (tu => dj, dh => lkgclxony);
  
  -- Single-driven assignments
  dj <= gacgky;
  q <= 12300.3_2_1_4_2 ms;
  gacgky <= 2#1_1_1_0_1# ms;
  
  -- Multi-driven assignments
  deilahlx <= 'W';
end ythh;

entity ahoii is
  port (lcx : in string(3 downto 2); mn : inout integer);
end ahoii;

architecture axse of ahoii is
  signal yhbbqzlbjw : real;
  signal mjoo : time;
  signal r : real;
  signal sejlxnu : real;
  signal jvcdivio : time;
begin
  cbgwekleb : entity work.ruvty
    port map (tu => jvcdivio, dh => sejlxnu);
  pqvqg : entity work.ruvty
    port map (tu => jvcdivio, dh => r);
  onedpumjf : entity work.ruvty
    port map (tu => mjoo, dh => yhbbqzlbjw);
  
  -- Single-driven assignments
  mn <= mn;
  jvcdivio <= 4 fs;
end axse;



-- Seed after: 9654738593134536012,5805648483995786113
