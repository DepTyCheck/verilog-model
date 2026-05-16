-- Seed: 868548779716535439,18424117564733761959



entity dsgkzlsqsu is
  port (sdwcj : linkage bit; px : in time);
end dsgkzlsqsu;



architecture tsvcbjmi of dsgkzlsqsu is
  
begin
  
end tsvcbjmi;



entity n is
  port (dfjvlfarzw : linkage time; ixsm : inout severity_level; jb : inout boolean; vwnjtzbeky : inout real);
end n;



architecture uw of n is
  
begin
  
end uw;



entity sanl is
  port (twzc : in integer);
end sanl;



architecture dh of sanl is
  signal xqyfyp : time;
  signal fefymikus : bit;
begin
  kbfrpdd : entity work.dsgkzlsqsu
    port map (sdwcj => fefymikus, px => xqyfyp);
end dh;

library ieee;
use ieee.std_logic_1164.all;

entity tcxwnd is
  port (ayfu : linkage time; vs : buffer std_logic; gfn : out bit; dpmpacxx : inout integer);
end tcxwnd;



architecture ucsaavht of tcxwnd is
  signal lujx : real;
  signal clibyebgv : boolean;
  signal jrownfdq : severity_level;
  signal nwwula : time;
  signal nnvmbk : integer;
begin
  ihcimks : entity work.sanl
    port map (twzc => dpmpacxx);
  alg : entity work.sanl
    port map (twzc => nnvmbk);
  qxia : entity work.n
    port map (dfjvlfarzw => nwwula, ixsm => jrownfdq, jb => clibyebgv, vwnjtzbeky => lujx);
end ucsaavht;



-- Seed after: 17091060347204567108,18424117564733761959
