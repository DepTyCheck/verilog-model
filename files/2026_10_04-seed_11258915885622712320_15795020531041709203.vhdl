-- Seed: 11258915885622712320,15795020531041709203

library ieee;
use ieee.std_logic_1164.all;

entity gbkz is
  port (yggj : linkage integer; bitnmletgy : inout std_logic_vector(0 downto 3); a : linkage time);
end gbkz;

architecture dqimdmyis of gbkz is
  
begin
  -- Multi-driven assignments
  bitnmletgy <= (others => '0');
end dqimdmyis;

entity rtjfzjlfl is
  port (jcelx : inout bit_vector(1 to 1));
end rtjfzjlfl;

library ieee;
use ieee.std_logic_1164.all;

architecture wqhjp of rtjfzjlfl is
  signal u : time;
  signal yflymgqd : integer;
  signal muxvswjly : time;
  signal ihlsnfku : std_logic_vector(0 downto 3);
  signal fxud : integer;
begin
  zoxaevt : entity work.gbkz
    port map (yggj => fxud, bitnmletgy => ihlsnfku, a => muxvswjly);
  jcpahr : entity work.gbkz
    port map (yggj => yflymgqd, bitnmletgy => ihlsnfku, a => u);
  
  -- Single-driven assignments
  jcelx <= jcelx;
  
  -- Multi-driven assignments
  ihlsnfku <= (others => '0');
  ihlsnfku <= ihlsnfku;
  ihlsnfku <= (others => '0');
  ihlsnfku <= ihlsnfku;
end wqhjp;

entity tzhghffdel is
  port (surya : out severity_level);
end tzhghffdel;

architecture njwkdobcz of tzhghffdel is
  signal mhmyvlrx : bit_vector(1 to 1);
  signal omyktcr : bit_vector(1 to 1);
begin
  ia : entity work.rtjfzjlfl
    port map (jcelx => omyktcr);
  hqjymoyua : entity work.rtjfzjlfl
    port map (jcelx => mhmyvlrx);
  
  -- Single-driven assignments
  surya <= surya;
end njwkdobcz;



-- Seed after: 5591218942408222133,15795020531041709203
