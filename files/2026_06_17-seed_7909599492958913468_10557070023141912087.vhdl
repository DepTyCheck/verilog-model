-- Seed: 7909599492958913468,10557070023141912087

library ieee;
use ieee.std_logic_1164.all;

entity uvapgox is
  port (vuka : inout std_logic_vector(0 to 2); ctywd : in string(3 downto 3); ex : in time; bdcqj : in integer);
end uvapgox;

architecture dsclnqd of uvapgox is
  
begin
  -- Multi-driven assignments
  vuka <= "0XX";
end dsclnqd;

entity r is
  port (oegazzko : linkage integer);
end r;

library ieee;
use ieee.std_logic_1164.all;

architecture lfgrlpd of r is
  signal lauysestx : std_logic_vector(0 to 2);
  signal bxevnxyo : integer;
  signal hv : time;
  signal flblrpoxj : string(3 downto 3);
  signal kzinopvz : integer;
  signal ozwyjh : time;
  signal txzd : string(3 downto 3);
  signal vao : std_logic_vector(0 to 2);
begin
  atypyfbays : entity work.uvapgox
    port map (vuka => vao, ctywd => txzd, ex => ozwyjh, bdcqj => kzinopvz);
  phvznz : entity work.uvapgox
    port map (vuka => vao, ctywd => flblrpoxj, ex => hv, bdcqj => bxevnxyo);
  brbpk : entity work.uvapgox
    port map (vuka => lauysestx, ctywd => flblrpoxj, ex => hv, bdcqj => kzinopvz);
  
  -- Single-driven assignments
  flblrpoxj <= (others => 'o');
  kzinopvz <= 8#1_4_5#;
  ozwyjh <= 16#BF2.C08B# ns;
  txzd <= (others => 'b');
  
  -- Multi-driven assignments
  lauysestx <= "-U0";
end lfgrlpd;



-- Seed after: 5429008275093312176,10557070023141912087
