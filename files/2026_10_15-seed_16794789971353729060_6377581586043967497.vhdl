-- Seed: 16794789971353729060,6377581586043967497

library ieee;
use ieee.std_logic_1164.all;

entity ysahc is
  port (xcvnlnejx : inout std_logic; csptk : in std_logic; uxiwlqcwj : inout time_vector(4 to 2));
end ysahc;

architecture pxswuuzty of ysahc is
  
begin
  -- Single-driven assignments
  uxiwlqcwj <= (others => 0 ns);
  
  -- Multi-driven assignments
  xcvnlnejx <= 'H';
  xcvnlnejx <= csptk;
  xcvnlnejx <= csptk;
  xcvnlnejx <= '-';
end pxswuuzty;

library ieee;
use ieee.std_logic_1164.all;

entity tkwhvpt is
  port (txvmvndm : inout integer; zatdm : in integer; evxzlde : buffer std_logic; ftf : in real_vector(2 downto 1));
end tkwhvpt;

library ieee;
use ieee.std_logic_1164.all;

architecture nczyophckw of tkwhvpt is
  signal stwoytq : time_vector(4 to 2);
  signal s : time_vector(4 to 2);
  signal rmwzrfnyn : std_logic;
  signal kvtjrd : time_vector(4 to 2);
  signal fmotjnp : std_logic;
  signal pmlbzt : time_vector(4 to 2);
begin
  fiswuabn : entity work.ysahc
    port map (xcvnlnejx => evxzlde, csptk => evxzlde, uxiwlqcwj => pmlbzt);
  bpzwtnr : entity work.ysahc
    port map (xcvnlnejx => evxzlde, csptk => fmotjnp, uxiwlqcwj => kvtjrd);
  vdtkqwo : entity work.ysahc
    port map (xcvnlnejx => rmwzrfnyn, csptk => rmwzrfnyn, uxiwlqcwj => s);
  cxuopj : entity work.ysahc
    port map (xcvnlnejx => evxzlde, csptk => evxzlde, uxiwlqcwj => stwoytq);
  
  -- Single-driven assignments
  txvmvndm <= 16#D_5_B_D#;
  
  -- Multi-driven assignments
  evxzlde <= evxzlde;
end nczyophckw;

library ieee;
use ieee.std_logic_1164.all;

entity ruwnzgbc is
  port (v : linkage integer; icmfnio : inout time; ucdn : inout std_logic_vector(1 to 0));
end ruwnzgbc;

library ieee;
use ieee.std_logic_1164.all;

architecture uyrwc of ruwnzgbc is
  signal i : integer;
  signal qewwlfcai : time_vector(4 to 2);
  signal hxbv : time_vector(4 to 2);
  signal w : std_logic;
  signal o : std_logic;
  signal avmr : real_vector(2 downto 1);
  signal jefoe : std_logic;
  signal bntczhcca : integer;
  signal zysyuohjy : integer;
begin
  obzl : entity work.tkwhvpt
    port map (txvmvndm => zysyuohjy, zatdm => bntczhcca, evxzlde => jefoe, ftf => avmr);
  utlstqj : entity work.ysahc
    port map (xcvnlnejx => o, csptk => w, uxiwlqcwj => hxbv);
  dqlxgdm : entity work.ysahc
    port map (xcvnlnejx => jefoe, csptk => jefoe, uxiwlqcwj => qewwlfcai);
  httrokzol : entity work.tkwhvpt
    port map (txvmvndm => bntczhcca, zatdm => i, evxzlde => jefoe, ftf => avmr);
  
  -- Single-driven assignments
  i <= 103;
  icmfnio <= icmfnio;
  
  -- Multi-driven assignments
  ucdn <= ucdn;
  ucdn <= "";
  ucdn <= "";
end uyrwc;

library ieee;
use ieee.std_logic_1164.all;

entity nkxvp is
  port (kipyywnetp : in std_logic_vector(2 to 2); hcnch : in std_logic; guehfz : linkage integer; nxqfeon : inout time);
end nkxvp;

library ieee;
use ieee.std_logic_1164.all;

architecture ttrzx of nkxvp is
  signal vtjmn : real_vector(2 downto 1);
  signal wzzy : std_logic;
  signal tyfmituwop : integer;
  signal mphesu : integer;
  signal u : std_logic_vector(1 to 0);
begin
  lyqcrvvuaj : entity work.ruwnzgbc
    port map (v => guehfz, icmfnio => nxqfeon, ucdn => u);
  pyvfapq : entity work.tkwhvpt
    port map (txvmvndm => mphesu, zatdm => tyfmituwop, evxzlde => wzzy, ftf => vtjmn);
  ebxncormlb : entity work.tkwhvpt
    port map (txvmvndm => tyfmituwop, zatdm => mphesu, evxzlde => wzzy, ftf => vtjmn);
  
  -- Single-driven assignments
  vtjmn <= (3.13234, 42.4_1_1_0_1);
  
  -- Multi-driven assignments
  u <= u;
  wzzy <= hcnch;
end ttrzx;



-- Seed after: 10520251829220586497,6377581586043967497
