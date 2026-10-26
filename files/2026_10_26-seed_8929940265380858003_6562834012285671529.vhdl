-- Seed: 8929940265380858003,6562834012285671529

library ieee;
use ieee.std_logic_1164.all;

entity pwt is
  port (kyhat : linkage std_logic; ysyydko : inout bit_vector(4 to 1));
end pwt;

architecture jrgb of pwt is
  
begin
  -- Single-driven assignments
  ysyydko <= ysyydko;
end jrgb;

library ieee;
use ieee.std_logic_1164.all;

entity za is
  port (jyey : out real; xus : linkage std_logic; rgkvaqlzkg : in bit);
end za;

library ieee;
use ieee.std_logic_1164.all;

architecture ccfnjcg of za is
  signal bvhdkczzxk : bit_vector(4 to 1);
  signal n : std_logic;
  signal djmkrhgkl : bit_vector(4 to 1);
  signal eanlokuor : std_logic;
  signal jsyyi : bit_vector(4 to 1);
  signal d : bit_vector(4 to 1);
  signal jjuw : std_logic;
begin
  bwb : entity work.pwt
    port map (kyhat => jjuw, ysyydko => d);
  pqfxsafwmg : entity work.pwt
    port map (kyhat => xus, ysyydko => jsyyi);
  sraidxhek : entity work.pwt
    port map (kyhat => eanlokuor, ysyydko => djmkrhgkl);
  gmyvqnmwze : entity work.pwt
    port map (kyhat => n, ysyydko => bvhdkczzxk);
  
  -- Single-driven assignments
  jyey <= jyey;
  
  -- Multi-driven assignments
  n <= jjuw;
  eanlokuor <= jjuw;
  jjuw <= n;
  eanlokuor <= jjuw;
end ccfnjcg;

library ieee;
use ieee.std_logic_1164.all;

entity vxolrbeihm is
  port (lxxryc : inout std_logic_vector(0 to 2));
end vxolrbeihm;

library ieee;
use ieee.std_logic_1164.all;

architecture eomsa of vxolrbeihm is
  signal vfs : bit_vector(4 to 1);
  signal ep : bit_vector(4 to 1);
  signal gfn : std_logic;
begin
  jslohpghpj : entity work.pwt
    port map (kyhat => gfn, ysyydko => ep);
  xmx : entity work.pwt
    port map (kyhat => gfn, ysyydko => vfs);
  
  -- Multi-driven assignments
  lxxryc <= lxxryc;
  lxxryc <= ('U', 'U', 'W');
end eomsa;



-- Seed after: 289701262401740004,6562834012285671529
