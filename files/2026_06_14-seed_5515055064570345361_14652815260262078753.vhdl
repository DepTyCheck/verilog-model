-- Seed: 5515055064570345361,14652815260262078753

entity ooaqoj is
  port (da : inout time; gzj : out real);
end ooaqoj;

architecture wv of ooaqoj is
  
begin
  
end wv;

library ieee;
use ieee.std_logic_1164.all;

entity pwxheqh is
  port (akfz : in std_logic_vector(2 to 3));
end pwxheqh;

architecture pxlzvh of pwxheqh is
  signal mwpvhr : real;
  signal tobjnc : time;
  signal bsojgayay : real;
  signal asmayfj : time;
begin
  vodh : entity work.ooaqoj
    port map (da => asmayfj, gzj => bsojgayay);
  n : entity work.ooaqoj
    port map (da => tobjnc, gzj => mwpvhr);
end pxlzvh;

entity xfq is
  port (e : inout character; zmuvn : buffer integer_vector(0 to 2); hue : linkage real);
end xfq;

library ieee;
use ieee.std_logic_1164.all;

architecture joabuagkwz of xfq is
  signal fsvc : std_logic_vector(2 to 3);
begin
  mdhytss : entity work.pwxheqh
    port map (akfz => fsvc);
  
  -- Single-driven assignments
  e <= 'q';
  zmuvn <= (2#00010#, 8#5_3_6_7#, 8#6_2_5#);
  
  -- Multi-driven assignments
  fsvc <= ('Z', '-');
end joabuagkwz;



-- Seed after: 17989000694512547295,14652815260262078753
