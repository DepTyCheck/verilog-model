-- Seed: 257812821819602648,17234720251424330329

library ieee;
use ieee.std_logic_1164.all;

entity icpufqii is
  port (pudpg : inout std_logic_vector(0 downto 4); aobfysbrt : out std_logic);
end icpufqii;

architecture jzjo of icpufqii is
  
begin
  -- Multi-driven assignments
  aobfysbrt <= 'X';
end jzjo;

entity uquacj is
  port (cvttru : out string(2 to 2));
end uquacj;

library ieee;
use ieee.std_logic_1164.all;

architecture xewmrl of uquacj is
  signal wakplyn : std_logic;
  signal cp : std_logic;
  signal izmebbfp : std_logic_vector(0 downto 4);
  signal iwlpquiv : std_logic;
  signal imtbo : std_logic_vector(0 downto 4);
begin
  bdnhjy : entity work.icpufqii
    port map (pudpg => imtbo, aobfysbrt => iwlpquiv);
  rdylatvek : entity work.icpufqii
    port map (pudpg => imtbo, aobfysbrt => iwlpquiv);
  cej : entity work.icpufqii
    port map (pudpg => izmebbfp, aobfysbrt => cp);
  imlep : entity work.icpufqii
    port map (pudpg => imtbo, aobfysbrt => wakplyn);
end xewmrl;

entity pslatji is
  port (fcpu : out time);
end pslatji;

library ieee;
use ieee.std_logic_1164.all;

architecture fmuq of pslatji is
  signal gjtwd : std_logic;
  signal tiow : std_logic;
  signal nywpwiug : std_logic;
  signal yfq : std_logic_vector(0 downto 4);
begin
  ppjaunwbx : entity work.icpufqii
    port map (pudpg => yfq, aobfysbrt => nywpwiug);
  jzbfk : entity work.icpufqii
    port map (pudpg => yfq, aobfysbrt => tiow);
  m : entity work.icpufqii
    port map (pudpg => yfq, aobfysbrt => gjtwd);
  
  -- Multi-driven assignments
  yfq <= "";
end fmuq;



-- Seed after: 1846792397519750712,17234720251424330329
