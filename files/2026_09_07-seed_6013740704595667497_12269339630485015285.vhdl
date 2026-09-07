-- Seed: 6013740704595667497,12269339630485015285

library ieee;
use ieee.std_logic_1164.all;

entity hhybfgar is
  port (k : in time; qi : inout std_logic_vector(4 downto 3); qeskrydl : linkage std_logic);
end hhybfgar;

architecture tlv of hhybfgar is
  
begin
  -- Multi-driven assignments
  qi <= qi;
end tlv;

entity yemridgdi is
  port (lnejptbc : in integer);
end yemridgdi;

library ieee;
use ieee.std_logic_1164.all;

architecture n of yemridgdi is
  signal c : std_logic;
  signal zonve : time;
  signal ru : std_logic_vector(4 downto 3);
  signal vnc : time;
  signal rn : std_logic;
  signal yftdlqdtlh : std_logic_vector(4 downto 3);
  signal gsxchb : time;
  signal akozdssg : std_logic;
  signal bqqirke : std_logic_vector(4 downto 3);
  signal nkkwvcsk : time;
begin
  hienyjsio : entity work.hhybfgar
    port map (k => nkkwvcsk, qi => bqqirke, qeskrydl => akozdssg);
  mzj : entity work.hhybfgar
    port map (k => gsxchb, qi => yftdlqdtlh, qeskrydl => rn);
  bkoxwwgu : entity work.hhybfgar
    port map (k => vnc, qi => ru, qeskrydl => akozdssg);
  kuuqludnr : entity work.hhybfgar
    port map (k => zonve, qi => bqqirke, qeskrydl => c);
  
  -- Single-driven assignments
  nkkwvcsk <= 2#1_0_0.0_0_1_0# us;
  gsxchb <= 16#640DC# fs;
  
  -- Multi-driven assignments
  bqqirke <= ('X', '1');
end n;



-- Seed after: 5334545393774784814,12269339630485015285
