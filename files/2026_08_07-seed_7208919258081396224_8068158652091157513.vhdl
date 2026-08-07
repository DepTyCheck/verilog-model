-- Seed: 7208919258081396224,8068158652091157513

library ieee;
use ieee.std_logic_1164.all;

entity mfcs is
  port (wbueoyzrql : in time; pwsevxko : in integer; fcvmh : out time; aqhnclo : out std_logic_vector(1 downto 4));
end mfcs;

architecture m of mfcs is
  
begin
  -- Single-driven assignments
  fcvmh <= 2#0_1# ps;
  
  -- Multi-driven assignments
  aqhnclo <= (others => '0');
  aqhnclo <= aqhnclo;
end m;

entity rtokjet is
  port (spgv : out time; qhvvcxld : inout real; nbalqzeno : linkage boolean; qkglziuz : buffer time_vector(4 downto 1));
end rtokjet;

library ieee;
use ieee.std_logic_1164.all;

architecture wuhjwzulbp of rtokjet is
  signal cp : time;
  signal yexvwbsml : integer;
  signal zdfujbsxk : time;
  signal eadae : std_logic_vector(1 downto 4);
  signal kehbjumw : integer;
  signal kajad : time;
begin
  pljuahdh : entity work.mfcs
    port map (wbueoyzrql => kajad, pwsevxko => kehbjumw, fcvmh => spgv, aqhnclo => eadae);
  jlcwr : entity work.mfcs
    port map (wbueoyzrql => zdfujbsxk, pwsevxko => kehbjumw, fcvmh => zdfujbsxk, aqhnclo => eadae);
  t : entity work.mfcs
    port map (wbueoyzrql => spgv, pwsevxko => yexvwbsml, fcvmh => kajad, aqhnclo => eadae);
  aumj : entity work.mfcs
    port map (wbueoyzrql => cp, pwsevxko => kehbjumw, fcvmh => cp, aqhnclo => eadae);
  
  -- Single-driven assignments
  qkglziuz <= qkglziuz;
  qhvvcxld <= 3_0.2_3_1;
  
  -- Multi-driven assignments
  eadae <= "";
end wuhjwzulbp;



-- Seed after: 16194993616140690046,8068158652091157513
