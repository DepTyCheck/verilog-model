-- Seed: 1578687532994988008,6290177331721581829

use std.reflection.all;

entity hsnkvcpbw is
  port (wptcsz : inout floating_subtype_mirror; meqvx : inout enumeration_value_mirror; dcsbygqzov : out real);
end hsnkvcpbw;

architecture lujmmertgh of hsnkvcpbw is
  
begin
  
end lujmmertgh;

entity dxfhxh is
  port (hipl : out bit; gmch : out integer);
end dxfhxh;

architecture ghmcuc of dxfhxh is
  
begin
  -- Single-driven assignments
  gmch <= gmch;
  hipl <= hipl;
end ghmcuc;

use std.reflection.all;

entity uumsjxsrz is
  port (yjdpztux : inout file_subtype_mirror; nxpx : inout floating_subtype_mirror; jvpurzm : inout integer_value_mirror);
end uumsjxsrz;

architecture ebffeijui of uumsjxsrz is
  
begin
  
end ebffeijui;

library ieee;
use ieee.std_logic_1164.all;

entity wvonvnc is
  port (wyzt : buffer integer; r : out std_logic_vector(1 to 4));
end wvonvnc;

use std.reflection.all;

architecture jb of wvonvnc is
  signal sczvb : bit;
  signal irqttisv : real;
  shared variable cycktzzj : enumeration_value_mirror;
  shared variable zdl : floating_subtype_mirror;
  signal tbe : real;
  shared variable pfdcg : enumeration_value_mirror;
  shared variable rntfrpby : floating_subtype_mirror;
  signal scnkic : integer;
  signal uynesfpy : bit;
begin
  psnegbu : entity work.dxfhxh
    port map (hipl => uynesfpy, gmch => scnkic);
  mmzo : entity work.hsnkvcpbw
    port map (wptcsz => rntfrpby, meqvx => pfdcg, dcsbygqzov => tbe);
  pasd : entity work.hsnkvcpbw
    port map (wptcsz => zdl, meqvx => cycktzzj, dcsbygqzov => irqttisv);
  zyxukmakn : entity work.dxfhxh
    port map (hipl => sczvb, gmch => wyzt);
  
  -- Multi-driven assignments
  r <= r;
  r <= ('X', 'X', '1', 'X');
end jb;



-- Seed after: 6388524615867268007,6290177331721581829
