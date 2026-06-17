-- Seed: 4889452119180295369,10557070023141912087

entity i is
  port (cikhtj : buffer real; nydmano : buffer real_vector(2 to 2); paov : out integer; dspho : linkage real);
end i;

architecture pf of i is
  
begin
  -- Single-driven assignments
  paov <= 30;
  nydmano <= (others => 8#36615.7170#);
end pf;

entity plilct is
  port (xhw : inout real; znml : in bit_vector(2 to 0); q : buffer integer; hywc : linkage boolean);
end plilct;

architecture ondk of plilct is
  signal ze : real;
  signal tak : real_vector(2 to 2);
begin
  srvyj : entity work.i
    port map (cikhtj => xhw, nydmano => tak, paov => q, dspho => ze);
end ondk;

library ieee;
use ieee.std_logic_1164.all;

entity ivsasqlh is
  port (znmifamm : linkage time; srlcq : inout std_logic);
end ivsasqlh;

architecture cqnm of ivsasqlh is
  signal ffgojckn : real;
  signal tpwjuukx : integer;
  signal y : real_vector(2 to 2);
  signal v : real;
begin
  tvmyhsgr : entity work.i
    port map (cikhtj => v, nydmano => y, paov => tpwjuukx, dspho => ffgojckn);
  
  -- Multi-driven assignments
  srlcq <= '-';
  srlcq <= 'X';
  srlcq <= 'X';
  srlcq <= 'H';
end cqnm;



-- Seed after: 17751856883534429934,10557070023141912087
