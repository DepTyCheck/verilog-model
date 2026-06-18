-- Seed: 12932540663428140531,8118127366649987907

library ieee;
use ieee.std_logic_1164.all;

entity xtwn is
  port (pjlv : linkage integer; spcteqamct : inout time_vector(4 to 1); k : buffer boolean; xqrhwai : inout std_logic);
end xtwn;

architecture mnb of xtwn is
  
begin
  -- Single-driven assignments
  k <= FALSE;
  spcteqamct <= (others => 0 ns);
  
  -- Multi-driven assignments
  xqrhwai <= '1';
  xqrhwai <= 'H';
end mnb;

entity fgnel is
  port (b : in character);
end fgnel;

library ieee;
use ieee.std_logic_1164.all;

architecture wpimqxokb of fgnel is
  signal zivbdb : boolean;
  signal nqwbjj : time_vector(4 to 1);
  signal ofvtdjho : integer;
  signal wegjgo : boolean;
  signal i : time_vector(4 to 1);
  signal eolfyl : integer;
  signal bsjmw : std_logic;
  signal amrw : boolean;
  signal ex : time_vector(4 to 1);
  signal xqpsk : integer;
begin
  an : entity work.xtwn
    port map (pjlv => xqpsk, spcteqamct => ex, k => amrw, xqrhwai => bsjmw);
  uil : entity work.xtwn
    port map (pjlv => eolfyl, spcteqamct => i, k => wegjgo, xqrhwai => bsjmw);
  gj : entity work.xtwn
    port map (pjlv => ofvtdjho, spcteqamct => nqwbjj, k => zivbdb, xqrhwai => bsjmw);
  
  -- Multi-driven assignments
  bsjmw <= 'L';
  bsjmw <= 'W';
  bsjmw <= 'W';
end wpimqxokb;



-- Seed after: 14161784683554999943,8118127366649987907
