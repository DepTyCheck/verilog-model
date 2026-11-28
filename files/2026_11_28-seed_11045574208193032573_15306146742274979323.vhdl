-- Seed: 11045574208193032573,15306146742274979323

entity ouepsm is
  port (swrfybmh : linkage bit_vector(0 downto 3));
end ouepsm;

architecture mos of ouepsm is
  
begin
  
end mos;

library ieee;
use ieee.std_logic_1164.all;

entity hxerhmoe is
  port (dbkiphceag : linkage std_logic_vector(4 downto 1); aecvuus : inout boolean; wlpdetl : inout real_vector(4 downto 2));
end hxerhmoe;

architecture qgc of hxerhmoe is
  signal uu : bit_vector(0 downto 3);
  signal i : bit_vector(0 downto 3);
  signal chs : bit_vector(0 downto 3);
begin
  jkqilj : entity work.ouepsm
    port map (swrfybmh => chs);
  vbsx : entity work.ouepsm
    port map (swrfybmh => i);
  gzivqp : entity work.ouepsm
    port map (swrfybmh => uu);
  
  -- Single-driven assignments
  wlpdetl <= wlpdetl;
  aecvuus <= FALSE;
end qgc;

entity unmzrxy is
  port (t : linkage time);
end unmzrxy;

library ieee;
use ieee.std_logic_1164.all;

architecture cseam of unmzrxy is
  signal aj : real_vector(4 downto 2);
  signal ighbcz : boolean;
  signal o : std_logic_vector(4 downto 1);
  signal evat : bit_vector(0 downto 3);
  signal npnmenjlj : bit_vector(0 downto 3);
  signal w : bit_vector(0 downto 3);
begin
  otyhlmwg : entity work.ouepsm
    port map (swrfybmh => w);
  dpctu : entity work.ouepsm
    port map (swrfybmh => npnmenjlj);
  cjckhkcjn : entity work.ouepsm
    port map (swrfybmh => evat);
  mziysvv : entity work.hxerhmoe
    port map (dbkiphceag => o, aecvuus => ighbcz, wlpdetl => aj);
  
  -- Multi-driven assignments
  o <= "01-W";
  o <= o;
  o <= o;
  o <= o;
end cseam;



-- Seed after: 859473635983323569,15306146742274979323
