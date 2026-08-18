-- Seed: 5590655265139689458,5983430343285687595

library ieee;
use ieee.std_logic_1164.all;

entity atdxqx is
  port (ktpkcsyupk : buffer std_logic_vector(2 to 0); wzlbxaoe : in integer_vector(4 downto 3); kolcmdcen : out time; gudnj : in integer);
end atdxqx;

architecture qovzr of atdxqx is
  
begin
  -- Single-driven assignments
  kolcmdcen <= kolcmdcen;
  
  -- Multi-driven assignments
  ktpkcsyupk <= ktpkcsyupk;
end qovzr;

entity vpw is
  port (hmvoxgty : in real; vbhs : inout bit; jzhhlbxhe : in real; n : in integer);
end vpw;

library ieee;
use ieee.std_logic_1164.all;

architecture qzqtlne of vpw is
  signal qw : time;
  signal hehmntdfl : time;
  signal jcnoebdzu : std_logic_vector(2 to 0);
  signal sfjair : integer;
  signal fgbkxawuya : time;
  signal z : integer_vector(4 downto 3);
  signal tjisrwwsn : integer;
  signal vltuoo : time;
  signal vc : integer_vector(4 downto 3);
  signal kqkj : std_logic_vector(2 to 0);
begin
  vlpcaolcd : entity work.atdxqx
    port map (ktpkcsyupk => kqkj, wzlbxaoe => vc, kolcmdcen => vltuoo, gudnj => tjisrwwsn);
  jscbs : entity work.atdxqx
    port map (ktpkcsyupk => kqkj, wzlbxaoe => z, kolcmdcen => fgbkxawuya, gudnj => sfjair);
  zproasml : entity work.atdxqx
    port map (ktpkcsyupk => jcnoebdzu, wzlbxaoe => vc, kolcmdcen => hehmntdfl, gudnj => tjisrwwsn);
  ypmv : entity work.atdxqx
    port map (ktpkcsyupk => kqkj, wzlbxaoe => z, kolcmdcen => qw, gudnj => n);
  
  -- Single-driven assignments
  sfjair <= n;
  
  -- Multi-driven assignments
  kqkj <= (others => '0');
end qzqtlne;

library ieee;
use ieee.std_logic_1164.all;

entity vxi is
  port (dfn : buffer std_logic; tno : inout severity_level; ketugp : inout time; rksgnxim : out std_logic_vector(3 downto 3));
end vxi;

library ieee;
use ieee.std_logic_1164.all;

architecture jwfnvse of vxi is
  signal luoxwiemwb : real;
  signal merkjd : bit;
  signal fwbrybula : real;
  signal pkxep : time;
  signal iy : std_logic_vector(2 to 0);
  signal cx : integer;
  signal ijg : time;
  signal xsjiu : integer_vector(4 downto 3);
  signal tu : std_logic_vector(2 to 0);
begin
  lu : entity work.atdxqx
    port map (ktpkcsyupk => tu, wzlbxaoe => xsjiu, kolcmdcen => ijg, gudnj => cx);
  udqalf : entity work.atdxqx
    port map (ktpkcsyupk => iy, wzlbxaoe => xsjiu, kolcmdcen => ketugp, gudnj => cx);
  o : entity work.atdxqx
    port map (ktpkcsyupk => tu, wzlbxaoe => xsjiu, kolcmdcen => pkxep, gudnj => cx);
  vuaxj : entity work.vpw
    port map (hmvoxgty => fwbrybula, vbhs => merkjd, jzhhlbxhe => luoxwiemwb, n => cx);
  
  -- Single-driven assignments
  tno <= ERROR;
  luoxwiemwb <= 8#3_6.455#;
  cx <= cx;
  xsjiu <= (120, 1_3_3_4_2);
  
  -- Multi-driven assignments
  rksgnxim <= rksgnxim;
end jwfnvse;

entity seisdutwyi is
  port (ttkrrkqfs : out integer; aefgegk : linkage boolean);
end seisdutwyi;

library ieee;
use ieee.std_logic_1164.all;

architecture yjfg of seisdutwyi is
  signal vaia : integer;
  signal yzytzgrfa : time;
  signal lrxiedfukx : integer;
  signal xnqt : time;
  signal xxkonmw : integer_vector(4 downto 3);
  signal dnctxssb : std_logic_vector(2 to 0);
  signal cbsuqoo : std_logic_vector(3 downto 3);
  signal cgorock : time;
  signal tujh : severity_level;
  signal xjjkfmjyr : std_logic;
begin
  q : entity work.vxi
    port map (dfn => xjjkfmjyr, tno => tujh, ketugp => cgorock, rksgnxim => cbsuqoo);
  nimkxlupp : entity work.atdxqx
    port map (ktpkcsyupk => dnctxssb, wzlbxaoe => xxkonmw, kolcmdcen => xnqt, gudnj => lrxiedfukx);
  elcg : entity work.atdxqx
    port map (ktpkcsyupk => dnctxssb, wzlbxaoe => xxkonmw, kolcmdcen => yzytzgrfa, gudnj => vaia);
  
  -- Single-driven assignments
  ttkrrkqfs <= 2#1_1_0#;
  lrxiedfukx <= lrxiedfukx;
  xxkonmw <= (2#11110#, 8#4_2_2_5_6#);
  
  -- Multi-driven assignments
  cbsuqoo <= cbsuqoo;
  dnctxssb <= (others => '0');
end yjfg;



-- Seed after: 2716778127659168013,5983430343285687595
