-- Seed: 11598477347567548907,5362988889992816031



entity dqtxw is
  port (p : linkage time; m : in integer; tbpi : linkage integer);
end dqtxw;



architecture skump of dqtxw is
  
begin
  
end skump;



entity bwoicjpypg is
  port (eqq : out real; q : in real);
end bwoicjpypg;



architecture aedxofwli of bwoicjpypg is
  signal olnbyzbuyg : integer;
  signal rimeoe : time;
  signal gxddujjnr : integer;
  signal rc : integer;
  signal uda : integer;
  signal rdcofjymu : time;
begin
  dvezvgpax : entity work.dqtxw
    port map (p => rdcofjymu, m => uda, tbpi => rc);
  n : entity work.dqtxw
    port map (p => rdcofjymu, m => gxddujjnr, tbpi => uda);
  lfgheyc : entity work.dqtxw
    port map (p => rimeoe, m => olnbyzbuyg, tbpi => gxddujjnr);
  cgyogcqb : entity work.dqtxw
    port map (p => rimeoe, m => gxddujjnr, tbpi => uda);
end aedxofwli;



entity yzdpiczgi is
  port (cgutdygga : in time; sqhv : buffer time; uji : buffer integer);
end yzdpiczgi;



architecture cobup of yzdpiczgi is
  signal ufuvc : real;
  signal bo : real;
  signal cxty : integer;
  signal bujckep : time;
begin
  t : entity work.dqtxw
    port map (p => bujckep, m => cxty, tbpi => cxty);
  g : entity work.bwoicjpypg
    port map (eqq => bo, q => ufuvc);
end cobup;

library ieee;
use ieee.std_logic_1164.all;

entity z is
  port (pfeaewx : linkage integer; qomsraavan : out std_logic; off : in time; ijjrsm : linkage integer);
end z;



architecture ct of z is
  signal sew : integer;
  signal oiligh : integer;
  signal utiaxyb : time;
begin
  wqpehlijlv : entity work.yzdpiczgi
    port map (cgutdygga => off, sqhv => utiaxyb, uji => oiligh);
  tp : entity work.dqtxw
    port map (p => off, m => sew, tbpi => ijjrsm);
end ct;



-- Seed after: 3527659119255678845,5362988889992816031
