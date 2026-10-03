-- Seed: 5638860453306303377,6140041381800297705

entity kaidg is
  port (nyisfqb : out integer);
end kaidg;

architecture pvandkhntb of kaidg is
  
begin
  -- Single-driven assignments
  nyisfqb <= nyisfqb;
end pvandkhntb;

entity ujfiovt is
  port (xi : linkage severity_level; bvufque : buffer integer);
end ujfiovt;

architecture p of ujfiovt is
  signal htymawg : integer;
  signal sxylrq : integer;
begin
  cebrbni : entity work.kaidg
    port map (nyisfqb => sxylrq);
  gxfgxgkf : entity work.kaidg
    port map (nyisfqb => bvufque);
  nyhhvbhbkv : entity work.kaidg
    port map (nyisfqb => htymawg);
end p;

entity aoz is
  port (jdwhcqdu : in integer_vector(4 to 1));
end aoz;

architecture ykmq of aoz is
  signal fkxzi : integer;
  signal sjbkzjy : severity_level;
begin
  x : entity work.ujfiovt
    port map (xi => sjbkzjy, bvufque => fkxzi);
end ykmq;

library ieee;
use ieee.std_logic_1164.all;

entity fylaxoo is
  port (zupxi : inout std_logic_vector(1 downto 0); nbnhw : in std_logic; suo : inout std_logic_vector(4 downto 2));
end fylaxoo;

architecture jyquanectv of fylaxoo is
  signal krdfyewow : integer_vector(4 to 1);
  signal lcpibhm : integer;
begin
  dsiazahfzl : entity work.kaidg
    port map (nyisfqb => lcpibhm);
  zf : entity work.aoz
    port map (jdwhcqdu => krdfyewow);
  
  -- Multi-driven assignments
  suo <= ('Z', '1', 'H');
  suo <= suo;
  suo <= ('H', 'U', 'W');
end jyquanectv;



-- Seed after: 17680729721911682111,6140041381800297705
