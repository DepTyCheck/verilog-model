-- Seed: 6027957918857064370,10940991575366938685

library ieee;
use ieee.std_logic_1164.all;

entity mfb is
  port (qqxyfrf : out time; outfuxcmce : out std_logic_vector(4 to 2); hvdmcgj : linkage integer);
end mfb;

architecture fjenpkkmek of mfb is
  
begin
  -- Single-driven assignments
  qqxyfrf <= 3_0_3.11143 fs;
end fjenpkkmek;

library ieee;
use ieee.std_logic_1164.all;

entity tizbjjvh is
  port (dzbbbqbvlt : out integer_vector(0 downto 2); adkkw : in std_logic);
end tizbjjvh;

library ieee;
use ieee.std_logic_1164.all;

architecture ssexen of tizbjjvh is
  signal biesnlql : integer;
  signal uwatgesz : time;
  signal fptnlmca : integer;
  signal tqjmzujxb : time;
  signal s : integer;
  signal n : std_logic_vector(4 to 2);
  signal sasjsfqgkn : time;
begin
  neajsyrj : entity work.mfb
    port map (qqxyfrf => sasjsfqgkn, outfuxcmce => n, hvdmcgj => s);
  rziynoh : entity work.mfb
    port map (qqxyfrf => tqjmzujxb, outfuxcmce => n, hvdmcgj => fptnlmca);
  dahyx : entity work.mfb
    port map (qqxyfrf => uwatgesz, outfuxcmce => n, hvdmcgj => biesnlql);
  
  -- Single-driven assignments
  dzbbbqbvlt <= dzbbbqbvlt;
  
  -- Multi-driven assignments
  n <= (others => '0');
  n <= n;
end ssexen;

entity f is
  port (aglnyqa : buffer integer; qthaippr : inout bit_vector(1 to 2));
end f;

library ieee;
use ieee.std_logic_1164.all;

architecture tjelbvaxfw of f is
  signal wozmcpnq : std_logic;
  signal yxcfvhgpbc : integer_vector(0 downto 2);
begin
  jhpn : entity work.tizbjjvh
    port map (dzbbbqbvlt => yxcfvhgpbc, adkkw => wozmcpnq);
  
  -- Single-driven assignments
  aglnyqa <= 16#B_A#;
  qthaippr <= qthaippr;
end tjelbvaxfw;



-- Seed after: 2978931519741174600,10940991575366938685
