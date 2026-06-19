-- Seed: 12094934100107029752,3108530264173481209

library ieee;
use ieee.std_logic_1164.all;

entity qtwnrlj is
  port (rtpgnymyau : inout real; zldszhzvht : buffer std_logic);
end qtwnrlj;

architecture nxjjk of qtwnrlj is
  
begin
  -- Multi-driven assignments
  zldszhzvht <= 'U';
  zldszhzvht <= 'L';
end nxjjk;

library ieee;
use ieee.std_logic_1164.all;

entity lm is
  port (aorsxhkgck : out real; hs : in std_logic);
end lm;

library ieee;
use ieee.std_logic_1164.all;

architecture mqtftbrp of lm is
  signal jayni : std_logic;
  signal utll : real;
  signal gjcrdc : std_logic;
begin
  xj : entity work.qtwnrlj
    port map (rtpgnymyau => aorsxhkgck, zldszhzvht => gjcrdc);
  fofmkojv : entity work.qtwnrlj
    port map (rtpgnymyau => utll, zldszhzvht => jayni);
  
  -- Multi-driven assignments
  gjcrdc <= '1';
  jayni <= '-';
  gjcrdc <= '-';
  gjcrdc <= 'Z';
end mqtftbrp;

library ieee;
use ieee.std_logic_1164.all;

entity pietqwba is
  port (z : out std_logic_vector(2 to 4); j : linkage real; rhfmznlekk : buffer real; uxb : inout std_logic);
end pietqwba;

library ieee;
use ieee.std_logic_1164.all;

architecture exkst of pietqwba is
  signal x : real;
  signal ivus : std_logic;
begin
  crm : entity work.lm
    port map (aorsxhkgck => rhfmznlekk, hs => ivus);
  ffrcfpppbw : entity work.qtwnrlj
    port map (rtpgnymyau => x, zldszhzvht => uxb);
  
  -- Multi-driven assignments
  uxb <= 'L';
  z <= "WUU";
end exkst;



-- Seed after: 545060814803914258,3108530264173481209
