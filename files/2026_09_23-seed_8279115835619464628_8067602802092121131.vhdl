-- Seed: 8279115835619464628,8067602802092121131

library ieee;
use ieee.std_logic_1164.all;

entity godre is
  port (qrhdbob : in std_logic; u : in std_logic_vector(4 downto 2); dzihewprov : buffer std_logic_vector(1 downto 0); ghrftgwslr : in std_logic);
end godre;

architecture a of godre is
  
begin
  
end a;

library ieee;
use ieee.std_logic_1164.all;

entity twsoyz is
  port (krix : linkage std_logic_vector(4 downto 2); yge : in integer);
end twsoyz;

library ieee;
use ieee.std_logic_1164.all;

architecture w of twsoyz is
  signal bcrluiial : std_logic;
  signal zvczl : std_logic_vector(1 downto 0);
  signal wx : std_logic;
  signal seqhgpxhv : std_logic;
  signal ng : std_logic_vector(1 downto 0);
  signal gvauljoqn : std_logic_vector(4 downto 2);
  signal sjiaoos : std_logic;
begin
  dbn : entity work.godre
    port map (qrhdbob => sjiaoos, u => gvauljoqn, dzihewprov => ng, ghrftgwslr => sjiaoos);
  espd : entity work.godre
    port map (qrhdbob => seqhgpxhv, u => gvauljoqn, dzihewprov => ng, ghrftgwslr => wx);
  dzagjuufar : entity work.godre
    port map (qrhdbob => sjiaoos, u => gvauljoqn, dzihewprov => zvczl, ghrftgwslr => bcrluiial);
  
  -- Multi-driven assignments
  bcrluiial <= sjiaoos;
end w;

entity bivnfqu is
  port (av : in time);
end bivnfqu;

library ieee;
use ieee.std_logic_1164.all;

architecture s of bivnfqu is
  signal vt : std_logic;
  signal wjbnqxxasd : std_logic_vector(1 downto 0);
  signal hraxgq : std_logic;
  signal utyhbb : integer;
  signal p : std_logic_vector(4 downto 2);
begin
  xsnkk : entity work.twsoyz
    port map (krix => p, yge => utyhbb);
  zozwwiongv : entity work.godre
    port map (qrhdbob => hraxgq, u => p, dzihewprov => wjbnqxxasd, ghrftgwslr => vt);
  
  -- Single-driven assignments
  utyhbb <= 4120;
end s;

entity scqjzquxg is
  port (zqmnpiklmr : inout boolean_vector(1 downto 2); wwnklbsbal : buffer integer_vector(0 downto 1));
end scqjzquxg;

library ieee;
use ieee.std_logic_1164.all;

architecture az of scqjzquxg is
  signal gioyiffki : time;
  signal o : integer;
  signal jsqhdtu : std_logic_vector(4 downto 2);
  signal n : std_logic;
  signal g : std_logic;
  signal ng : std_logic_vector(1 downto 0);
  signal cg : std_logic_vector(4 downto 2);
  signal whgcbd : std_logic;
begin
  v : entity work.godre
    port map (qrhdbob => whgcbd, u => cg, dzihewprov => ng, ghrftgwslr => g);
  jn : entity work.godre
    port map (qrhdbob => n, u => cg, dzihewprov => ng, ghrftgwslr => whgcbd);
  kq : entity work.twsoyz
    port map (krix => jsqhdtu, yge => o);
  vnfd : entity work.bivnfqu
    port map (av => gioyiffki);
  
  -- Multi-driven assignments
  whgcbd <= whgcbd;
  jsqhdtu <= ('L', '1', '0');
end az;



-- Seed after: 6550084234615630786,8067602802092121131
