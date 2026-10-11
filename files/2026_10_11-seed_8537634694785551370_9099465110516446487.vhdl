-- Seed: 8537634694785551370,9099465110516446487

library ieee;
use ieee.std_logic_1164.all;

entity cchxqedteq is
  port (orvdgb : linkage std_logic; lvdb : in time; zrb : linkage real; whso : inout std_logic_vector(0 downto 4));
end cchxqedteq;

architecture mlbp of cchxqedteq is
  
begin
  
end mlbp;

entity bwbxttb is
  port (rhjwfutb : inout time);
end bwbxttb;

library ieee;
use ieee.std_logic_1164.all;

architecture hqsx of bwbxttb is
  signal aqlxo : std_logic_vector(0 downto 4);
  signal gihbtc : real;
  signal c : time;
  signal g : std_logic;
  signal tcafb : real;
  signal yuia : time;
  signal cjwxhttjt : real;
  signal uafwyeuvkb : time;
  signal qflryrkcur : std_logic_vector(0 downto 4);
  signal lw : real;
  signal begku : std_logic;
begin
  bymugrn : entity work.cchxqedteq
    port map (orvdgb => begku, lvdb => rhjwfutb, zrb => lw, whso => qflryrkcur);
  tlrgk : entity work.cchxqedteq
    port map (orvdgb => begku, lvdb => uafwyeuvkb, zrb => cjwxhttjt, whso => qflryrkcur);
  sfyvwfz : entity work.cchxqedteq
    port map (orvdgb => begku, lvdb => yuia, zrb => tcafb, whso => qflryrkcur);
  yyeedf : entity work.cchxqedteq
    port map (orvdgb => g, lvdb => c, zrb => gihbtc, whso => aqlxo);
  
  -- Single-driven assignments
  uafwyeuvkb <= 16#5_B_3_4.1_5_6_3_0# fs;
  c <= rhjwfutb;
  rhjwfutb <= 3_1_0 ps;
  yuia <= yuia;
  
  -- Multi-driven assignments
  begku <= begku;
  begku <= 'L';
  begku <= 'H';
  qflryrkcur <= (others => '0');
end hqsx;

entity aadnwdzwr is
  port (led : buffer integer; lk : buffer integer);
end aadnwdzwr;

architecture iphhgdq of aadnwdzwr is
  
begin
  -- Single-driven assignments
  lk <= 4;
  led <= lk;
end iphhgdq;



-- Seed after: 1270014181571968357,9099465110516446487
