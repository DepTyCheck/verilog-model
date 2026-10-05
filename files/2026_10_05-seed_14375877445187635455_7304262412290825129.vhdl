-- Seed: 14375877445187635455,7304262412290825129

entity naliaxki is
  port (kolbvhm : buffer integer; rgzuxshfo : in integer; tvdvo : buffer time);
end naliaxki;

architecture hseyxhfd of naliaxki is
  
begin
  
end hseyxhfd;

library ieee;
use ieee.std_logic_1164.all;

entity v is
  port (pfuy : buffer real; zytur : out time; yypmfsgbh : buffer std_logic_vector(4 downto 2); oca : inout integer);
end v;

architecture mqsqmh of v is
  signal ozoefzntei : integer;
  signal nj : integer;
  signal xwv : time;
  signal lofewole : integer;
begin
  e : entity work.naliaxki
    port map (kolbvhm => oca, rgzuxshfo => lofewole, tvdvo => xwv);
  f : entity work.naliaxki
    port map (kolbvhm => nj, rgzuxshfo => ozoefzntei, tvdvo => zytur);
  
  -- Single-driven assignments
  ozoefzntei <= oca;
  lofewole <= 2#00001#;
  pfuy <= 16#3246.2_4_1_C_2#;
  
  -- Multi-driven assignments
  yypmfsgbh <= yypmfsgbh;
  yypmfsgbh <= "W1U";
end mqsqmh;



-- Seed after: 17922592370067961960,7304262412290825129
