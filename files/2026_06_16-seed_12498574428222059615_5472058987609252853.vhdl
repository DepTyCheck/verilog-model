-- Seed: 12498574428222059615,5472058987609252853

library ieee;
use ieee.std_logic_1164.all;

entity xj is
  port (gkkkh : out std_logic; venbavdyp : inout real);
end xj;

architecture r of xj is
  
begin
  -- Single-driven assignments
  venbavdyp <= 3_3_2_1_2.2_0_3_1;
  
  -- Multi-driven assignments
  gkkkh <= '0';
  gkkkh <= 'U';
end r;

library ieee;
use ieee.std_logic_1164.all;

entity swk is
  port (cxirqdds : in time; msxrzzsd : inout time; nbwhrtghjo : linkage std_logic; eawcgaytd : out boolean);
end swk;

library ieee;
use ieee.std_logic_1164.all;

architecture vpgv of swk is
  signal mum : real;
  signal cuftxp : std_logic;
  signal tcriqka : real;
  signal mypzdtzvj : real;
  signal jcfv : std_logic;
  signal qqnojrqn : real;
  signal beinr : std_logic;
begin
  vuczum : entity work.xj
    port map (gkkkh => beinr, venbavdyp => qqnojrqn);
  kocxek : entity work.xj
    port map (gkkkh => jcfv, venbavdyp => mypzdtzvj);
  pyc : entity work.xj
    port map (gkkkh => beinr, venbavdyp => tcriqka);
  txru : entity work.xj
    port map (gkkkh => cuftxp, venbavdyp => mum);
  
  -- Single-driven assignments
  msxrzzsd <= 0_1_3_1.3_2_0_1_1 ns;
  eawcgaytd <= TRUE;
  
  -- Multi-driven assignments
  beinr <= 'Z';
  beinr <= 'X';
  beinr <= '-';
  cuftxp <= 'L';
end vpgv;



-- Seed after: 4879930918740060166,5472058987609252853
