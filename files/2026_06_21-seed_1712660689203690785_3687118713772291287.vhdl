-- Seed: 1712660689203690785,3687118713772291287

entity fwckz is
  port (wshqjauz : buffer real);
end fwckz;

architecture rgwd of fwckz is
  
begin
  -- Single-driven assignments
  wshqjauz <= 132.204;
end rgwd;

library ieee;
use ieee.std_logic_1164.all;

entity pxkrk is
  port (vygseqnryv : linkage std_logic_vector(4 downto 2); p : linkage std_logic; tlkgszffil : out time; yg : in string(3 downto 4));
end pxkrk;

architecture kvpq of pxkrk is
  signal umwtc : real;
  signal uautj : real;
  signal dqmmprf : real;
  signal podakeyheu : real;
begin
  hszymgdwm : entity work.fwckz
    port map (wshqjauz => podakeyheu);
  kbbkumnua : entity work.fwckz
    port map (wshqjauz => dqmmprf);
  zqmpqgxyay : entity work.fwckz
    port map (wshqjauz => uautj);
  cnz : entity work.fwckz
    port map (wshqjauz => umwtc);
  
  -- Single-driven assignments
  tlkgszffil <= 2#00# ns;
end kvpq;

entity hbmtmsx is
  port (kxcf : buffer severity_level);
end hbmtmsx;

library ieee;
use ieee.std_logic_1164.all;

architecture ih of hbmtmsx is
  signal wefr : string(3 downto 4);
  signal b : time;
  signal yfvcyirpv : std_logic;
  signal eyfzftezzf : std_logic_vector(4 downto 2);
begin
  kiq : entity work.pxkrk
    port map (vygseqnryv => eyfzftezzf, p => yfvcyirpv, tlkgszffil => b, yg => wefr);
  
  -- Single-driven assignments
  wefr <= "";
  kxcf <= WARNING;
  
  -- Multi-driven assignments
  eyfzftezzf <= ('X', 'W', 'U');
  eyfzftezzf <= "UXL";
  eyfzftezzf <= ('X', 'X', 'L');
end ih;



-- Seed after: 11893182279527499656,3687118713772291287
