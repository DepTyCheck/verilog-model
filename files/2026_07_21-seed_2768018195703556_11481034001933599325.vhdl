-- Seed: 2768018195703556,11481034001933599325

library ieee;
use ieee.std_logic_1164.all;

entity pmgs is
  port (pxetklyvu : buffer integer; jwntlgm : in integer; pbfqvdurz : in std_logic);
end pmgs;

architecture d of pmgs is
  
begin
  -- Single-driven assignments
  pxetklyvu <= 0;
end d;

entity dfwmp is
  port (yk : inout character; yw : out integer_vector(1 downto 2); n : inout bit);
end dfwmp;

library ieee;
use ieee.std_logic_1164.all;

architecture q of dfwmp is
  signal dmrtp : integer;
  signal ieqyrmpm : std_logic;
  signal bugszoga : integer;
  signal bzmqko : integer;
  signal prmh : integer;
  signal oxmewyvdal : std_logic;
  signal iydshqmg : integer;
  signal fmh : integer;
begin
  ed : entity work.pmgs
    port map (pxetklyvu => fmh, jwntlgm => iydshqmg, pbfqvdurz => oxmewyvdal);
  zrqtuceshz : entity work.pmgs
    port map (pxetklyvu => iydshqmg, jwntlgm => prmh, pbfqvdurz => oxmewyvdal);
  jumdnt : entity work.pmgs
    port map (pxetklyvu => bzmqko, jwntlgm => bugszoga, pbfqvdurz => ieqyrmpm);
  db : entity work.pmgs
    port map (pxetklyvu => prmh, jwntlgm => dmrtp, pbfqvdurz => oxmewyvdal);
  
  -- Single-driven assignments
  n <= n;
  dmrtp <= fmh;
  yk <= yk;
  yw <= yw;
end q;

library ieee;
use ieee.std_logic_1164.all;

entity pbmsknkhr is
  port (ur : inout real_vector(1 to 4); cz : in real_vector(4 downto 1); n : out std_logic; whm : out std_logic);
end pbmsknkhr;

architecture oyszxc of pbmsknkhr is
  signal fccflnzc : integer;
begin
  a : entity work.pmgs
    port map (pxetklyvu => fccflnzc, jwntlgm => fccflnzc, pbfqvdurz => whm);
  
  -- Single-driven assignments
  ur <= (0_3_4.2220, 2#1_0_0_0_0.0011#, 16#F.752#, 31.3_2_4);
  
  -- Multi-driven assignments
  n <= whm;
  whm <= '-';
  n <= whm;
end oyszxc;



-- Seed after: 9950552104498354887,11481034001933599325
