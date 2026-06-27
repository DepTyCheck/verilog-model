-- Seed: 11318365710653197461,4860866131898729603

entity yrnwll is
  port (klbth : inout time);
end yrnwll;

architecture ivrx of yrnwll is
  
begin
  -- Single-driven assignments
  klbth <= 30.1 ps;
end ivrx;

entity ktwirbqg is
  port (odemg : inout integer; qm : inout time);
end ktwirbqg;

architecture bwatxwhn of ktwirbqg is
  signal yjisso : time;
  signal phyclvyu : time;
  signal psyfshern : time;
begin
  pkhfbjvdb : entity work.yrnwll
    port map (klbth => psyfshern);
  vrva : entity work.yrnwll
    port map (klbth => phyclvyu);
  gxrehwfkmn : entity work.yrnwll
    port map (klbth => qm);
  tywfzblhm : entity work.yrnwll
    port map (klbth => yjisso);
end bwatxwhn;

library ieee;
use ieee.std_logic_1164.all;

entity zvhz is
  port (ntvo : inout std_logic; akmro : linkage std_logic_vector(3 to 4); ttb : linkage std_logic; nowqkv : out character);
end zvhz;

architecture wein of zvhz is
  signal cmtfdlpl : time;
  signal xknueslqqw : integer;
  signal hust : time;
  signal xdy : time;
  signal pmal : time;
  signal gtnz : integer;
begin
  zgbsvhagj : entity work.ktwirbqg
    port map (odemg => gtnz, qm => pmal);
  ugnndybsu : entity work.yrnwll
    port map (klbth => xdy);
  qqlglbistp : entity work.yrnwll
    port map (klbth => hust);
  hhy : entity work.ktwirbqg
    port map (odemg => xknueslqqw, qm => cmtfdlpl);
  
  -- Multi-driven assignments
  ntvo <= 'W';
end wein;



-- Seed after: 2093173184526874668,4860866131898729603
