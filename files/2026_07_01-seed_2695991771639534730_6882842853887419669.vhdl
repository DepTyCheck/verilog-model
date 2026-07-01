-- Seed: 2695991771639534730,6882842853887419669

library ieee;
use ieee.std_logic_1164.all;

entity pbcw is
  port (oladq : in string(1 to 2); dimsrrex : buffer time; sbyqgxw : linkage std_logic; hvmsrwuv : in real);
end pbcw;

architecture vkejqphrtf of pbcw is
  
begin
  -- Single-driven assignments
  dimsrrex <= 4_0.41 ms;
end vkejqphrtf;

library ieee;
use ieee.std_logic_1164.all;

entity hjais is
  port (aknvamiw : buffer std_logic_vector(1 downto 2); dsm : out string(4 downto 3); twtyedq : in bit);
end hjais;

library ieee;
use ieee.std_logic_1164.all;

architecture enlqyyigg of hjais is
  signal pr : time;
  signal bqqsapvpd : string(1 to 2);
  signal iidboz : real;
  signal piwibkye : std_logic;
  signal iil : time;
begin
  mkawu : entity work.pbcw
    port map (oladq => dsm, dimsrrex => iil, sbyqgxw => piwibkye, hvmsrwuv => iidboz);
  lufhdljcrs : entity work.pbcw
    port map (oladq => bqqsapvpd, dimsrrex => pr, sbyqgxw => piwibkye, hvmsrwuv => iidboz);
  
  -- Single-driven assignments
  bqqsapvpd <= ('r', 't');
  iidboz <= 8#1.7#;
  dsm <= ('r', 's');
  
  -- Multi-driven assignments
  aknvamiw <= "";
  aknvamiw <= "";
end enlqyyigg;



-- Seed after: 2678768300554932832,6882842853887419669
