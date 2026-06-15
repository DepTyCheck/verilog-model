-- Seed: 14134987534861107769,1834764876137802293

library ieee;
use ieee.std_logic_1164.all;

entity uqhmc is
  port (tnf : buffer real; u : linkage bit; mkgdpxaxb : out std_logic_vector(4 downto 1); fj : linkage std_logic_vector(3 downto 1));
end uqhmc;

architecture g of uqhmc is
  
begin
  -- Single-driven assignments
  tnf <= 1_2_1_4_3.3;
  
  -- Multi-driven assignments
  mkgdpxaxb <= "1LWL";
end g;

entity xlodgy is
  port (tkwlney : buffer time);
end xlodgy;

library ieee;
use ieee.std_logic_1164.all;

architecture expzcvsd of xlodgy is
  signal rfukkyflw : std_logic_vector(3 downto 1);
  signal l : std_logic_vector(4 downto 1);
  signal jdajlcadf : bit;
  signal yqbeyu : real;
  signal hjywgq : std_logic_vector(4 downto 1);
  signal jwyuzav : bit;
  signal dhztdry : real;
  signal xozgk : std_logic_vector(3 downto 1);
  signal teduvo : std_logic_vector(4 downto 1);
  signal xlpu : bit;
  signal zumhczoh : real;
begin
  zgyjca : entity work.uqhmc
    port map (tnf => zumhczoh, u => xlpu, mkgdpxaxb => teduvo, fj => xozgk);
  aeed : entity work.uqhmc
    port map (tnf => dhztdry, u => jwyuzav, mkgdpxaxb => hjywgq, fj => xozgk);
  cjdfmr : entity work.uqhmc
    port map (tnf => yqbeyu, u => jdajlcadf, mkgdpxaxb => l, fj => rfukkyflw);
end expzcvsd;



-- Seed after: 16306079896111057772,1834764876137802293
