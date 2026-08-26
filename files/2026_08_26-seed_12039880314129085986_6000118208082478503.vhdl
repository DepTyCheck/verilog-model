-- Seed: 12039880314129085986,6000118208082478503

library ieee;
use ieee.std_logic_1164.all;

entity hfsxh is
  port (pkzcmd : buffer std_logic_vector(2 to 2); kecchq : linkage std_logic_vector(1 to 0); r : linkage severity_level);
end hfsxh;

architecture d of hfsxh is
  
begin
  -- Multi-driven assignments
  pkzcmd <= (others => 'Z');
  pkzcmd <= "U";
end d;

library ieee;
use ieee.std_logic_1164.all;

entity pderurdey is
  port (kpc : linkage std_logic);
end pderurdey;

architecture ihvnwtw of pderurdey is
  
begin
  
end ihvnwtw;

library ieee;
use ieee.std_logic_1164.all;

entity upfq is
  port (ygjaha : buffer std_logic_vector(3 downto 4); dzypihxx : out boolean);
end upfq;

library ieee;
use ieee.std_logic_1164.all;

architecture rpepxky of upfq is
  signal vvld : severity_level;
  signal lzdsnyn : severity_level;
  signal ftxdjl : std_logic_vector(1 to 0);
  signal eiza : severity_level;
  signal phbsdp : std_logic_vector(1 to 0);
  signal evwdarqy : severity_level;
  signal qgfr : std_logic_vector(2 to 2);
begin
  rnfcptfji : entity work.hfsxh
    port map (pkzcmd => qgfr, kecchq => ygjaha, r => evwdarqy);
  musljaojy : entity work.hfsxh
    port map (pkzcmd => qgfr, kecchq => phbsdp, r => eiza);
  dlq : entity work.hfsxh
    port map (pkzcmd => qgfr, kecchq => ftxdjl, r => lzdsnyn);
  eajdr : entity work.hfsxh
    port map (pkzcmd => qgfr, kecchq => ftxdjl, r => vvld);
  
  -- Single-driven assignments
  dzypihxx <= dzypihxx;
  
  -- Multi-driven assignments
  ygjaha <= (others => '0');
  ygjaha <= ygjaha;
  qgfr <= qgfr;
  ygjaha <= "";
end rpepxky;



-- Seed after: 16906091879490366064,6000118208082478503
