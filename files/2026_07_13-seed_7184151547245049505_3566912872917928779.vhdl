-- Seed: 7184151547245049505,3566912872917928779

use std.reflection.all;

entity pocbivchq is
  port (otz : inout enumeration_value_mirror; a : out integer_vector(4 downto 2); ndo : out time);
end pocbivchq;

architecture d of pocbivchq is
  
begin
  -- Single-driven assignments
  ndo <= 16#8DD# ps;
  a <= a;
end d;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity anhvq is
  port (c : inout value_mirror; lgituwonp : out std_logic; snfqe : inout floating_subtype_mirror);
end anhvq;

architecture jv of anhvq is
  
begin
  -- Multi-driven assignments
  lgituwonp <= '0';
end jv;

use std.reflection.all;

entity jbnl is
  port (ktqnuhz : inout file_subtype_mirror; xkt : inout subtype_mirror);
end jbnl;

use std.reflection.all;

architecture fh of jbnl is
  signal j : time;
  signal wuhz : integer_vector(4 downto 2);
  shared variable yhl : enumeration_value_mirror;
  signal xyrm : time;
  signal ei : integer_vector(4 downto 2);
  shared variable tfk : enumeration_value_mirror;
begin
  pxowcmjp : entity work.pocbivchq
    port map (otz => tfk, a => ei, ndo => xyrm);
  hpoeubi : entity work.pocbivchq
    port map (otz => yhl, a => wuhz, ndo => j);
end fh;

use std.reflection.all;

entity u is
  port (c : out integer; evxdj : inout array_subtype_mirror; rqza : in integer; aarrny : inout floating_subtype_mirror);
end u;

architecture xbkgagjlx of u is
  
begin
  -- Single-driven assignments
  c <= rqza;
end xbkgagjlx;



-- Seed after: 15066830242670953194,3566912872917928779
