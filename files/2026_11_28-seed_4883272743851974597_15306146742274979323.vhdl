-- Seed: 4883272743851974597,15306146742274979323

library ieee;
use ieee.std_logic_1164.all;

entity ilctnlywvk is
  port (lzjyplxr : linkage integer; wjpilfhg : inout std_logic; iyjx : buffer std_logic);
end ilctnlywvk;

architecture aewtyszh of ilctnlywvk is
  
begin
  -- Multi-driven assignments
  iyjx <= '-';
  iyjx <= iyjx;
end aewtyszh;

entity fxs is
  port (jjhgup : in time; hqzhbc : out real; whttomaux : inout time_vector(1 downto 0));
end fxs;

library ieee;
use ieee.std_logic_1164.all;

architecture xleknfkc of fxs is
  signal zvck : std_logic;
  signal ciswvpdkfe : std_logic;
  signal px : integer;
  signal dcg : std_logic;
  signal zxwztb : integer;
  signal mn : std_logic;
  signal qrjdalcob : std_logic;
  signal xufujjnc : integer;
  signal gdvdrefq : std_logic;
  signal ec : integer;
begin
  xypsjlpc : entity work.ilctnlywvk
    port map (lzjyplxr => ec, wjpilfhg => gdvdrefq, iyjx => gdvdrefq);
  hvaeqawspv : entity work.ilctnlywvk
    port map (lzjyplxr => xufujjnc, wjpilfhg => qrjdalcob, iyjx => mn);
  upsvycu : entity work.ilctnlywvk
    port map (lzjyplxr => zxwztb, wjpilfhg => dcg, iyjx => dcg);
  kxom : entity work.ilctnlywvk
    port map (lzjyplxr => px, wjpilfhg => ciswvpdkfe, iyjx => zvck);
  
  -- Single-driven assignments
  hqzhbc <= 16#0119.9_1_F#;
  whttomaux <= (2 us, 2#0001# fs);
  
  -- Multi-driven assignments
  gdvdrefq <= dcg;
  ciswvpdkfe <= '-';
  zvck <= mn;
  mn <= '1';
end xleknfkc;

library ieee;
use ieee.std_logic_1164.all;

entity hvitfgm is
  port (urffkqe : inout integer; hqwi : out integer_vector(3 downto 0); s : out std_logic);
end hvitfgm;

library ieee;
use ieee.std_logic_1164.all;

architecture jrdqcs of hvitfgm is
  signal rz : time_vector(1 downto 0);
  signal fzktda : real;
  signal fzc : time;
  signal tkf : std_logic;
  signal svgh : time_vector(1 downto 0);
  signal bkcq : real;
  signal mf : time;
begin
  wtskcwqsmy : entity work.fxs
    port map (jjhgup => mf, hqzhbc => bkcq, whttomaux => svgh);
  ryjjyuoilb : entity work.ilctnlywvk
    port map (lzjyplxr => urffkqe, wjpilfhg => s, iyjx => tkf);
  o : entity work.fxs
    port map (jjhgup => fzc, hqzhbc => fzktda, whttomaux => rz);
  
  -- Single-driven assignments
  mf <= 2#1# us;
  hqwi <= (0, 16#4_8_A#, 2#1#, 8#2_3_1_4_4#);
  fzc <= fzc;
  
  -- Multi-driven assignments
  tkf <= s;
end jrdqcs;

entity thshgnorp is
  port (y : linkage real_vector(1 downto 2); cwbsfmom : linkage integer; oney : buffer time);
end thshgnorp;

library ieee;
use ieee.std_logic_1164.all;

architecture fhcyuoeddg of thshgnorp is
  signal vsarrmro : std_logic;
  signal sqrpufkgx : std_logic;
  signal jubuchil : time_vector(1 downto 0);
  signal f : real;
  signal ymxfh : time;
  signal sfhbj : std_logic;
  signal jzdn : std_logic;
  signal tpbmt : integer;
begin
  upweihx : entity work.ilctnlywvk
    port map (lzjyplxr => tpbmt, wjpilfhg => jzdn, iyjx => sfhbj);
  sidheycsd : entity work.fxs
    port map (jjhgup => ymxfh, hqzhbc => f, whttomaux => jubuchil);
  tf : entity work.ilctnlywvk
    port map (lzjyplxr => cwbsfmom, wjpilfhg => sqrpufkgx, iyjx => vsarrmro);
end fhcyuoeddg;



-- Seed after: 3193920670233568632,15306146742274979323
