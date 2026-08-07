-- Seed: 14109241914701934916,8068158652091157513

entity qpsbgimkxp is
  port (e : linkage real; ktercpvz : buffer real; r : in real; w : linkage bit);
end qpsbgimkxp;

architecture hhfod of qpsbgimkxp is
  
begin
  -- Single-driven assignments
  ktercpvz <= r;
end hhfod;

library ieee;
use ieee.std_logic_1164.all;

entity y is
  port (qkqitth : inout std_logic);
end y;

architecture fug of y is
  signal rd : bit;
  signal ytfrcjt : real;
  signal jpuk : bit;
  signal ylpzsqz : real;
  signal sh : real;
  signal rfy : bit;
  signal k : real;
  signal gnpmpvte : real;
  signal hefv : real;
begin
  utmahs : entity work.qpsbgimkxp
    port map (e => hefv, ktercpvz => gnpmpvte, r => k, w => rfy);
  jbqxj : entity work.qpsbgimkxp
    port map (e => sh, ktercpvz => k, r => ylpzsqz, w => jpuk);
  xxtor : entity work.qpsbgimkxp
    port map (e => ytfrcjt, ktercpvz => ylpzsqz, r => ylpzsqz, w => rd);
end fug;

entity qbuj is
  port (knvg : in real);
end qbuj;

library ieee;
use ieee.std_logic_1164.all;

architecture k of qbuj is
  signal kdjm : bit;
  signal r : bit;
  signal ax : real;
  signal dzlalifa : real;
  signal nzk : real;
  signal nrszjdm : std_logic;
  signal zuj : bit;
  signal bgabokth : real;
  signal w : real;
  signal lc : real;
begin
  wklujbg : entity work.qpsbgimkxp
    port map (e => lc, ktercpvz => w, r => bgabokth, w => zuj);
  j : entity work.y
    port map (qkqitth => nrszjdm);
  dfddecy : entity work.qpsbgimkxp
    port map (e => nzk, ktercpvz => dzlalifa, r => ax, w => r);
  n : entity work.qpsbgimkxp
    port map (e => bgabokth, ktercpvz => ax, r => bgabokth, w => kdjm);
  
  -- Multi-driven assignments
  nrszjdm <= 'W';
  nrszjdm <= 'W';
  nrszjdm <= 'U';
end k;



-- Seed after: 10003462102778213496,8068158652091157513
