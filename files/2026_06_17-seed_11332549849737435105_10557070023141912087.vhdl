-- Seed: 11332549849737435105,10557070023141912087

entity qecpl is
  port (ro : out time);
end qecpl;

architecture ile of qecpl is
  
begin
  -- Single-driven assignments
  ro <= 12413 ns;
end ile;

entity wxrtnt is
  port (imgydk : in integer; zj : in time; batbsp : linkage integer);
end wxrtnt;

architecture jssxl of wxrtnt is
  
begin
  
end jssxl;

entity hgk is
  port (bxvsrwmkcr : inout boolean);
end hgk;

architecture vvlcw of hgk is
  signal png : time;
  signal gapaqp : time;
  signal wxr : time;
begin
  mnkmqoiful : entity work.qecpl
    port map (ro => wxr);
  ylrelfqvq : entity work.qecpl
    port map (ro => gapaqp);
  vvwes : entity work.qecpl
    port map (ro => png);
  
  -- Single-driven assignments
  bxvsrwmkcr <= FALSE;
end vvlcw;

library ieee;
use ieee.std_logic_1164.all;

entity cfwijbmnj is
  port (e : inout std_logic; kt : buffer bit_vector(0 downto 0));
end cfwijbmnj;

architecture poighxroe of cfwijbmnj is
  signal tyqrilz : integer;
  signal otjgppkb : time;
begin
  ilionw : entity work.qecpl
    port map (ro => otjgppkb);
  ciyxitayif : entity work.wxrtnt
    port map (imgydk => tyqrilz, zj => otjgppkb, batbsp => tyqrilz);
  
  -- Single-driven assignments
  kt <= (others => '0');
  
  -- Multi-driven assignments
  e <= 'U';
  e <= 'X';
end poighxroe;



-- Seed after: 7751583589504550070,10557070023141912087
