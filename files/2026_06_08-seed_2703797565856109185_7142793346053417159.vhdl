-- Seed: 2703797565856109185,7142793346053417159



entity tnko is
  port (nsliirm : inout real; olssbekhb : inout real; xcwp : out bit; zcoyd : linkage integer);
end tnko;



architecture pmhq of tnko is
  
begin
  
end pmhq;



entity wqqdk is
  port (bxdmralmhb : buffer real_vector(4 downto 3); wgkuinfjn : linkage real; q : linkage integer);
end wqqdk;



architecture pvd of wqqdk is
  signal fhmke : integer;
  signal hlfzogxr : bit;
  signal cizxo : real;
  signal rghkabxnah : real;
  signal tubgsnkhlu : integer;
  signal dsrkcwca : bit;
  signal umkpvnyf : real;
  signal m : real;
  signal htdyqmb : integer;
  signal fbbotpm : bit;
  signal evcniut : real;
  signal acp : real;
  signal vspeip : integer;
  signal ptwvquhpe : bit;
  signal gtzgzh : real;
  signal kef : real;
begin
  ivpjuqsj : entity work.tnko
    port map (nsliirm => kef, olssbekhb => gtzgzh, xcwp => ptwvquhpe, zcoyd => vspeip);
  atm : entity work.tnko
    port map (nsliirm => acp, olssbekhb => evcniut, xcwp => fbbotpm, zcoyd => htdyqmb);
  je : entity work.tnko
    port map (nsliirm => m, olssbekhb => umkpvnyf, xcwp => dsrkcwca, zcoyd => tubgsnkhlu);
  pc : entity work.tnko
    port map (nsliirm => rghkabxnah, olssbekhb => cizxo, xcwp => hlfzogxr, zcoyd => fhmke);
end pvd;



entity gfpey is
  port (haeydhir : in real);
end gfpey;



architecture yhyuwem of gfpey is
  signal wbnurfmb : integer;
  signal ae : bit;
  signal hd : real;
  signal ad : real;
begin
  zcu : entity work.tnko
    port map (nsliirm => ad, olssbekhb => hd, xcwp => ae, zcoyd => wbnurfmb);
end yhyuwem;

library ieee;
use ieee.std_logic_1164.all;

entity ac is
  port (m : linkage std_logic; tvhpimbm : inout real; ronmqugp : out std_logic_vector(4 to 2));
end ac;



architecture xki of ac is
  signal mtpa : real;
  signal gpnxvluhwi : integer;
  signal vlx : bit;
  signal osxaqpeiaa : real;
  signal jg : real;
begin
  achhaeky : entity work.tnko
    port map (nsliirm => jg, olssbekhb => osxaqpeiaa, xcwp => vlx, zcoyd => gpnxvluhwi);
  wtxu : entity work.gfpey
    port map (haeydhir => mtpa);
end xki;



-- Seed after: 5464352067398898791,7142793346053417159
