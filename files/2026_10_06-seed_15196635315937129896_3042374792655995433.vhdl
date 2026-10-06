-- Seed: 15196635315937129896,3042374792655995433

library ieee;
use ieee.std_logic_1164.all;

entity xjtfddw is
  port (cfyfrhcbti : in boolean_vector(4 downto 2); abscnoyp : inout std_logic; nbyzgsitmb : in real);
end xjtfddw;

architecture tlpc of xjtfddw is
  
begin
  -- Multi-driven assignments
  abscnoyp <= '-';
  abscnoyp <= 'X';
  abscnoyp <= abscnoyp;
end tlpc;

library ieee;
use ieee.std_logic_1164.all;

entity r is
  port (vvixp : out real; fk : inout std_logic; eih : inout time; cxk : buffer integer);
end r;

library ieee;
use ieee.std_logic_1164.all;

architecture djisphnqgz of r is
  signal kwqj : std_logic;
  signal hsvnzpw : boolean_vector(4 downto 2);
  signal pwxrijc : boolean_vector(4 downto 2);
begin
  wj : entity work.xjtfddw
    port map (cfyfrhcbti => pwxrijc, abscnoyp => fk, nbyzgsitmb => vvixp);
  kujp : entity work.xjtfddw
    port map (cfyfrhcbti => hsvnzpw, abscnoyp => kwqj, nbyzgsitmb => vvixp);
  jucmyyf : entity work.xjtfddw
    port map (cfyfrhcbti => pwxrijc, abscnoyp => fk, nbyzgsitmb => vvixp);
  n : entity work.xjtfddw
    port map (cfyfrhcbti => pwxrijc, abscnoyp => fk, nbyzgsitmb => vvixp);
  
  -- Single-driven assignments
  vvixp <= 16#5_1_9_5.E_9#;
  eih <= 16#D1# ms;
  cxk <= 16#A_A_9_A#;
end djisphnqgz;

library ieee;
use ieee.std_logic_1164.all;

entity klara is
  port (k : linkage std_logic; vwflpdu : in integer; ejereiik : in real);
end klara;

library ieee;
use ieee.std_logic_1164.all;

architecture cmqwabe of klara is
  signal w : real;
  signal xqygpq : std_logic;
  signal fmrbwrh : boolean_vector(4 downto 2);
begin
  zglwlfyyve : entity work.xjtfddw
    port map (cfyfrhcbti => fmrbwrh, abscnoyp => xqygpq, nbyzgsitmb => w);
  wfqad : entity work.xjtfddw
    port map (cfyfrhcbti => fmrbwrh, abscnoyp => xqygpq, nbyzgsitmb => ejereiik);
  
  -- Single-driven assignments
  fmrbwrh <= (TRUE, TRUE, TRUE);
  w <= ejereiik;
end cmqwabe;



-- Seed after: 13408452890817929085,3042374792655995433
