-- Seed: 1070360119849191633,12359743974512393525

library ieee;
use ieee.std_logic_1164.all;

entity hp is
  port (thi : inout std_logic_vector(2 downto 4); yi : in time; roaosa : buffer time);
end hp;

architecture quzv of hp is
  
begin
  -- Multi-driven assignments
  thi <= (others => '0');
end quzv;

entity b is
  port (vu : linkage integer; utp : out integer; kfrglfoohs : out time; xn : linkage real);
end b;

library ieee;
use ieee.std_logic_1164.all;

architecture wej of b is
  signal ecaykyeo : time;
  signal lvhulqup : time;
  signal elx : std_logic_vector(2 downto 4);
  signal rpyhnqyu : time;
  signal uzp : time;
  signal spvujxfa : std_logic_vector(2 downto 4);
  signal fvwnrt : time;
  signal bcpkunw : time;
  signal egwecru : std_logic_vector(2 downto 4);
begin
  fskugo : entity work.hp
    port map (thi => egwecru, yi => bcpkunw, roaosa => fvwnrt);
  jffw : entity work.hp
    port map (thi => spvujxfa, yi => uzp, roaosa => rpyhnqyu);
  ifknqrk : entity work.hp
    port map (thi => elx, yi => lvhulqup, roaosa => ecaykyeo);
  
  -- Single-driven assignments
  uzp <= 2#0# ns;
  kfrglfoohs <= 16#D.8_9_E_F_6# ps;
end wej;



-- Seed after: 13328404415914368442,12359743974512393525
