-- Seed: 2959039509675345439,13843488114570579517

library ieee;
use ieee.std_logic_1164.all;

entity yy is
  port (lyhqpaxvrn : out std_logic; rpucnac : buffer time_vector(4 to 3); bdpwx : buffer real);
end yy;

architecture snfog of yy is
  
begin
  -- Single-driven assignments
  bdpwx <= bdpwx;
  rpucnac <= rpucnac;
end snfog;

entity hhvr is
  port (xa : buffer real; lh : buffer time; v : in bit_vector(0 to 4));
end hhvr;

library ieee;
use ieee.std_logic_1164.all;

architecture onfe of hhvr is
  signal derjbrbvbr : real;
  signal lbgay : time_vector(4 to 3);
  signal ifncznyc : std_logic;
  signal swmjjcu : time_vector(4 to 3);
  signal vus : std_logic;
begin
  xemlmtd : entity work.yy
    port map (lyhqpaxvrn => vus, rpucnac => swmjjcu, bdpwx => xa);
  smmvvzpeut : entity work.yy
    port map (lyhqpaxvrn => ifncznyc, rpucnac => lbgay, bdpwx => derjbrbvbr);
  
  -- Multi-driven assignments
  vus <= ifncznyc;
  vus <= vus;
  vus <= vus;
  vus <= '1';
end onfe;

entity qwrvosye is
  port (zmbyhutag : buffer time; esr : linkage real; nkgnhvwgp : buffer time);
end qwrvosye;

library ieee;
use ieee.std_logic_1164.all;

architecture biszixtd of qwrvosye is
  signal huuf : real;
  signal gkriocwsn : time_vector(4 to 3);
  signal jjxuyiaclg : std_logic;
  signal efcd : real;
  signal nrueone : time_vector(4 to 3);
  signal xusfrilbua : std_logic;
begin
  tpbkwkdgk : entity work.yy
    port map (lyhqpaxvrn => xusfrilbua, rpucnac => nrueone, bdpwx => efcd);
  xco : entity work.yy
    port map (lyhqpaxvrn => jjxuyiaclg, rpucnac => gkriocwsn, bdpwx => huuf);
  
  -- Single-driven assignments
  nkgnhvwgp <= nkgnhvwgp;
  
  -- Multi-driven assignments
  xusfrilbua <= xusfrilbua;
  xusfrilbua <= 'U';
  xusfrilbua <= 'Z';
end biszixtd;

library ieee;
use ieee.std_logic_1164.all;

entity kkh is
  port (ghlvy : in time; k : buffer std_logic; ujrraoxpkw : linkage bit);
end kkh;

architecture dbgsoasct of kkh is
  signal agi : real;
  signal exa : time_vector(4 to 3);
  signal aaacso : bit_vector(0 to 4);
  signal rwaqffvnz : time;
  signal cl : real;
begin
  dkehfz : entity work.hhvr
    port map (xa => cl, lh => rwaqffvnz, v => aaacso);
  m : entity work.yy
    port map (lyhqpaxvrn => k, rpucnac => exa, bdpwx => agi);
  
  -- Single-driven assignments
  aaacso <= ('0', '0', '0', '0', '1');
  
  -- Multi-driven assignments
  k <= '1';
  k <= k;
end dbgsoasct;



-- Seed after: 6813352848810645594,13843488114570579517
