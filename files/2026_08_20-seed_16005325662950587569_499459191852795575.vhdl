-- Seed: 16005325662950587569,499459191852795575

entity tqdxhee is
  port (eemcnutr : in integer_vector(2 downto 4));
end tqdxhee;

architecture yk of tqdxhee is
  
begin
  
end yk;

library ieee;
use ieee.std_logic_1164.all;

entity xk is
  port (qjznv : inout std_logic; jybr : out time; wzot : in character);
end xk;

architecture njqbudeat of xk is
  signal pvbwqrkel : integer_vector(2 downto 4);
  signal ey : integer_vector(2 downto 4);
begin
  qw : entity work.tqdxhee
    port map (eemcnutr => ey);
  lmi : entity work.tqdxhee
    port map (eemcnutr => pvbwqrkel);
  
  -- Single-driven assignments
  pvbwqrkel <= ey;
  ey <= (others => 0);
  jybr <= jybr;
  
  -- Multi-driven assignments
  qjznv <= 'W';
end njqbudeat;

library ieee;
use ieee.std_logic_1164.all;

entity evuptvbhc is
  port (maogbm : out time; sxroqh : inout std_logic; ogrlwqbm : out time; ukoosvin : out std_logic_vector(2 downto 0));
end evuptvbhc;

architecture dzoadjyowr of evuptvbhc is
  
begin
  -- Single-driven assignments
  ogrlwqbm <= 3 sec;
  maogbm <= 8#1.2# ns;
  
  -- Multi-driven assignments
  sxroqh <= '-';
  ukoosvin <= ukoosvin;
  sxroqh <= '1';
end dzoadjyowr;

library ieee;
use ieee.std_logic_1164.all;

entity y is
  port (mr : in std_logic_vector(4 to 1); efktu : in real; dqx : in integer; zalhmmjbf : in std_logic);
end y;

library ieee;
use ieee.std_logic_1164.all;

architecture mmu of y is
  signal oodp : std_logic_vector(2 downto 0);
  signal ukx : time;
  signal qolkvmyy : time;
  signal u : character;
  signal fqn : time;
  signal tset : std_logic;
  signal ihbdqubslk : integer_vector(2 downto 4);
  signal zzqd : integer_vector(2 downto 4);
begin
  djiibhtv : entity work.tqdxhee
    port map (eemcnutr => zzqd);
  dgtk : entity work.tqdxhee
    port map (eemcnutr => ihbdqubslk);
  yp : entity work.xk
    port map (qjznv => tset, jybr => fqn, wzot => u);
  kxrpj : entity work.evuptvbhc
    port map (maogbm => qolkvmyy, sxroqh => tset, ogrlwqbm => ukx, ukoosvin => oodp);
  
  -- Single-driven assignments
  zzqd <= zzqd;
  u <= u;
  ihbdqubslk <= zzqd;
end mmu;



-- Seed after: 6405099474492458007,499459191852795575
