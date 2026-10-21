-- Seed: 4668669392964885453,17219977430298397867

library ieee;
use ieee.std_logic_1164.all;

entity bzyevmp is
  port (t : linkage real; ffjcir : inout time; er : in std_logic_vector(4 downto 3); pga : inout bit);
end bzyevmp;

architecture btcgmqjd of bzyevmp is
  
begin
  -- Single-driven assignments
  ffjcir <= ffjcir;
  pga <= '1';
end btcgmqjd;

entity zylho is
  port (lgnyhyai : out time; vytosjrpj : in bit_vector(3 downto 2));
end zylho;

library ieee;
use ieee.std_logic_1164.all;

architecture my of zylho is
  signal rft : bit;
  signal jizowtqje : std_logic_vector(4 downto 3);
  signal xghhbze : time;
  signal snuncm : real;
  signal d : bit;
  signal q : std_logic_vector(4 downto 3);
  signal zdvpw : real;
  signal seojx : bit;
  signal xcfyj : std_logic_vector(4 downto 3);
  signal vomfomgmq : time;
  signal eohuf : real;
  signal iwwzpf : bit;
  signal i : std_logic_vector(4 downto 3);
  signal lsvczf : time;
  signal ekv : real;
begin
  bxrornbyf : entity work.bzyevmp
    port map (t => ekv, ffjcir => lsvczf, er => i, pga => iwwzpf);
  qrqb : entity work.bzyevmp
    port map (t => eohuf, ffjcir => vomfomgmq, er => xcfyj, pga => seojx);
  fandlbund : entity work.bzyevmp
    port map (t => zdvpw, ffjcir => lgnyhyai, er => q, pga => d);
  tkks : entity work.bzyevmp
    port map (t => snuncm, ffjcir => xghhbze, er => jizowtqje, pga => rft);
end my;

entity qtqa is
  port (jidjcli : out time; vsq : inout real; cvi : buffer real);
end qtqa;

library ieee;
use ieee.std_logic_1164.all;

architecture yhted of qtqa is
  signal s : bit;
  signal mrm : time;
  signal uf : bit;
  signal gjjq : std_logic_vector(4 downto 3);
  signal wiyfgz : time;
  signal ly : bit_vector(3 downto 2);
  signal kwuqlzpggi : time;
begin
  c : entity work.zylho
    port map (lgnyhyai => kwuqlzpggi, vytosjrpj => ly);
  ua : entity work.bzyevmp
    port map (t => vsq, ffjcir => wiyfgz, er => gjjq, pga => uf);
  nrqg : entity work.zylho
    port map (lgnyhyai => jidjcli, vytosjrpj => ly);
  ugtby : entity work.bzyevmp
    port map (t => cvi, ffjcir => mrm, er => gjjq, pga => s);
  
  -- Single-driven assignments
  ly <= ly;
  
  -- Multi-driven assignments
  gjjq <= gjjq;
  gjjq <= "X1";
  gjjq <= gjjq;
  gjjq <= gjjq;
end yhted;



-- Seed after: 409390222031265754,17219977430298397867
