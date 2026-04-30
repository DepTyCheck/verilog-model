-- Seed: 5739267518145991845,12368583144879391087



entity bmb is
  port (ykifazoxa : in integer; kvkofqrzul : inout time);
end bmb;



architecture dpi of bmb is
  
begin
  
end dpi;

library ieee;
use ieee.std_logic_1164.all;

entity qtsfpi is
  port (rnvxkvgd : linkage real; kwufydq : linkage std_logic; ursbdyzj : in std_logic);
end qtsfpi;



architecture tgrqxmjeeg of qtsfpi is
  signal pwxglpofxf : time;
  signal k : time;
  signal c : integer;
begin
  pnwxil : entity work.bmb
    port map (ykifazoxa => c, kvkofqrzul => k);
  l : entity work.bmb
    port map (ykifazoxa => c, kvkofqrzul => pwxglpofxf);
end tgrqxmjeeg;



entity ysbqgvmu is
  port (xuqimftya : linkage integer; wqmmr : inout real);
end ysbqgvmu;

library ieee;
use ieee.std_logic_1164.all;

architecture k of ysbqgvmu is
  signal eqp : time;
  signal gxznih : integer;
  signal kqmcmegb : std_logic;
  signal sdtvupho : std_logic;
  signal gqktm : std_logic;
  signal xtzuvgzwy : real;
begin
  p : entity work.qtsfpi
    port map (rnvxkvgd => xtzuvgzwy, kwufydq => gqktm, ursbdyzj => sdtvupho);
  vfahn : entity work.qtsfpi
    port map (rnvxkvgd => wqmmr, kwufydq => kqmcmegb, ursbdyzj => gqktm);
  uqcajh : entity work.bmb
    port map (ykifazoxa => gxznih, kvkofqrzul => eqp);
end k;



-- Seed after: 14776998464972277442,12368583144879391087
