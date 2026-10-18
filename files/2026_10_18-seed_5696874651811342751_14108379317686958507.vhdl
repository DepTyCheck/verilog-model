-- Seed: 5696874651811342751,14108379317686958507

library ieee;
use ieee.std_logic_1164.all;

entity khopmm is
  port (qkuwpnysw : in std_logic_vector(4 downto 2); bbd : inout integer_vector(2 downto 1); bagvgn : inout time; twooxgm : in integer);
end khopmm;

architecture evgjckjk of khopmm is
  
begin
  
end evgjckjk;

library ieee;
use ieee.std_logic_1164.all;

entity pp is
  port (bokabyxv : buffer std_logic_vector(3 to 1));
end pp;

library ieee;
use ieee.std_logic_1164.all;

architecture kiartocr of pp is
  signal bnsauwx : integer;
  signal zkfiuse : time;
  signal fnnobm : integer_vector(2 downto 1);
  signal vmzz : std_logic_vector(4 downto 2);
begin
  tbebh : entity work.khopmm
    port map (qkuwpnysw => vmzz, bbd => fnnobm, bagvgn => zkfiuse, twooxgm => bnsauwx);
  
  -- Single-driven assignments
  bnsauwx <= 3013;
  
  -- Multi-driven assignments
  bokabyxv <= "";
end kiartocr;

entity g is
  port (qz : linkage time_vector(4 downto 3); t : out severity_level; qhogwccqzw : linkage integer; mjp : inout severity_level);
end g;

architecture brxlufupm of g is
  
begin
  -- Single-driven assignments
  mjp <= mjp;
  t <= ERROR;
end brxlufupm;

library ieee;
use ieee.std_logic_1164.all;

entity vyrqazuv is
  port (rtl : linkage integer; cp : buffer std_logic);
end vyrqazuv;

library ieee;
use ieee.std_logic_1164.all;

architecture vc of vyrqazuv is
  signal afrtse : severity_level;
  signal vdgje : integer;
  signal xuzeg : severity_level;
  signal x : time_vector(4 downto 3);
  signal yfnqmo : integer;
  signal uxxqcrayec : time;
  signal dly : integer_vector(2 downto 1);
  signal xoklst : std_logic_vector(4 downto 2);
  signal tu : std_logic_vector(3 to 1);
begin
  hftbnuioh : entity work.pp
    port map (bokabyxv => tu);
  qbsrqyjl : entity work.pp
    port map (bokabyxv => tu);
  nmnmqnkxk : entity work.khopmm
    port map (qkuwpnysw => xoklst, bbd => dly, bagvgn => uxxqcrayec, twooxgm => yfnqmo);
  cbelyxokuh : entity work.g
    port map (qz => x, t => xuzeg, qhogwccqzw => vdgje, mjp => afrtse);
  
  -- Multi-driven assignments
  cp <= cp;
end vc;



-- Seed after: 13936256506011578718,14108379317686958507
