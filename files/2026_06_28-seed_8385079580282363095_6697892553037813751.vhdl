-- Seed: 8385079580282363095,6697892553037813751

library ieee;
use ieee.std_logic_1164.all;

entity ilxnjmu is
  port (zpahdvqgz : out std_logic_vector(0 downto 2); bstfesss : inout std_logic_vector(3 to 3); xux : out time);
end ilxnjmu;

architecture quxhwks of ilxnjmu is
  
begin
  -- Single-driven assignments
  xux <= 16#B_A# ps;
  
  -- Multi-driven assignments
  bstfesss <= "-";
  bstfesss <= "U";
end quxhwks;

library ieee;
use ieee.std_logic_1164.all;

entity j is
  port (uc : out std_logic; gjvjtsiqzf : linkage real; mcxyyslqa : linkage integer);
end j;

library ieee;
use ieee.std_logic_1164.all;

architecture vqt of j is
  signal uma : time;
  signal bhckwkb : std_logic_vector(0 downto 2);
  signal jq : time;
  signal iczt : time;
  signal sixxeqdd : std_logic_vector(3 to 3);
  signal tvkvtrtw : std_logic_vector(0 downto 2);
begin
  oabdffhz : entity work.ilxnjmu
    port map (zpahdvqgz => tvkvtrtw, bstfesss => sixxeqdd, xux => iczt);
  xptfqfein : entity work.ilxnjmu
    port map (zpahdvqgz => tvkvtrtw, bstfesss => sixxeqdd, xux => jq);
  rj : entity work.ilxnjmu
    port map (zpahdvqgz => bhckwkb, bstfesss => sixxeqdd, xux => uma);
  
  -- Multi-driven assignments
  uc <= '-';
  bhckwkb <= (others => '0');
  tvkvtrtw <= "";
end vqt;



-- Seed after: 1116004119237006740,6697892553037813751
