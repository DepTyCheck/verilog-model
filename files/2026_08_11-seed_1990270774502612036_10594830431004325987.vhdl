-- Seed: 1990270774502612036,10594830431004325987

library ieee;
use ieee.std_logic_1164.all;

entity xpsh is
  port (whyzo : inout integer; zz : linkage std_logic_vector(4 to 2); yaz : out time; aw : out time);
end xpsh;

architecture byvqydc of xpsh is
  
begin
  -- Single-driven assignments
  aw <= 16#1_1_2_9.0_B_8_5_4# ps;
  whyzo <= 133;
  yaz <= aw;
end byvqydc;

library ieee;
use ieee.std_logic_1164.all;

entity bpi is
  port (xhdxio : linkage std_logic; w : in std_logic_vector(4 to 0); bv : out std_logic);
end bpi;

library ieee;
use ieee.std_logic_1164.all;

architecture jrcgpb of bpi is
  signal wj : time;
  signal ruxwgsowyv : time;
  signal cr : integer;
  signal wzagv : time;
  signal xqwogktsd : time;
  signal gqizofv : std_logic_vector(4 to 2);
  signal cm : integer;
  signal ft : time;
  signal jkcpsc : time;
  signal zopeht : std_logic_vector(4 to 2);
  signal cnzvvswza : integer;
begin
  f : entity work.xpsh
    port map (whyzo => cnzvvswza, zz => zopeht, yaz => jkcpsc, aw => ft);
  ty : entity work.xpsh
    port map (whyzo => cm, zz => gqizofv, yaz => xqwogktsd, aw => wzagv);
  ayrhw : entity work.xpsh
    port map (whyzo => cr, zz => w, yaz => ruxwgsowyv, aw => wj);
  
  -- Multi-driven assignments
  bv <= 'W';
  gqizofv <= w;
  bv <= bv;
  bv <= '0';
end jrcgpb;

entity milqz is
  port (xste : out real; cwxeqw : linkage integer);
end milqz;

library ieee;
use ieee.std_logic_1164.all;

architecture xnjsr of milqz is
  signal hpfbszwliu : std_logic;
  signal bkfpjmulk : time;
  signal wgcyd : time;
  signal xf : std_logic_vector(4 to 2);
  signal wpyc : integer;
  signal cafu : time;
  signal gwvqlthnl : time;
  signal zddv : std_logic_vector(4 to 0);
  signal mqo : integer;
begin
  mnuayxq : entity work.xpsh
    port map (whyzo => mqo, zz => zddv, yaz => gwvqlthnl, aw => cafu);
  fodjjqlg : entity work.xpsh
    port map (whyzo => wpyc, zz => xf, yaz => wgcyd, aw => bkfpjmulk);
  i : entity work.bpi
    port map (xhdxio => hpfbszwliu, w => zddv, bv => hpfbszwliu);
  
  -- Single-driven assignments
  xste <= 34111.4;
  
  -- Multi-driven assignments
  zddv <= (others => '0');
  zddv <= zddv;
end xnjsr;



-- Seed after: 17730241970648404047,10594830431004325987
