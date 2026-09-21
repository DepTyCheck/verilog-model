-- Seed: 2981005397927459210,12143220691580258643

entity ulx is
  port (ifs : out severity_level);
end ulx;

architecture jpslbrg of ulx is
  
begin
  
end jpslbrg;

library ieee;
use ieee.std_logic_1164.all;

entity rdv is
  port (iuhhwtlilz : inout time_vector(3 to 1); rechlo : buffer bit; ld : out std_logic_vector(0 downto 0); xewel : inout std_logic_vector(3 to 2));
end rdv;

architecture mijafs of rdv is
  signal xzdexjnfzi : severity_level;
  signal bjrrpmsmgo : severity_level;
  signal hsrylota : severity_level;
  signal vae : severity_level;
begin
  lsd : entity work.ulx
    port map (ifs => vae);
  vibf : entity work.ulx
    port map (ifs => hsrylota);
  ywe : entity work.ulx
    port map (ifs => bjrrpmsmgo);
  lbekz : entity work.ulx
    port map (ifs => xzdexjnfzi);
  
  -- Single-driven assignments
  iuhhwtlilz <= (others => 0 ns);
end mijafs;

entity z is
  port (llnzvvo : inout time);
end z;

library ieee;
use ieee.std_logic_1164.all;

architecture ijxm of z is
  signal o : std_logic_vector(3 to 2);
  signal rdcxqjntz : bit;
  signal ujhsfc : time_vector(3 to 1);
  signal xnfowvuoz : severity_level;
  signal wdppf : std_logic_vector(0 downto 0);
  signal crgxggmj : bit;
  signal wviocnv : time_vector(3 to 1);
  signal gg : std_logic_vector(3 to 2);
  signal nttsw : std_logic_vector(0 downto 0);
  signal kigpnmjql : bit;
  signal m : time_vector(3 to 1);
begin
  ra : entity work.rdv
    port map (iuhhwtlilz => m, rechlo => kigpnmjql, ld => nttsw, xewel => gg);
  aefm : entity work.rdv
    port map (iuhhwtlilz => wviocnv, rechlo => crgxggmj, ld => wdppf, xewel => gg);
  w : entity work.ulx
    port map (ifs => xnfowvuoz);
  frnkobjmzh : entity work.rdv
    port map (iuhhwtlilz => ujhsfc, rechlo => rdcxqjntz, ld => nttsw, xewel => o);
  
  -- Single-driven assignments
  llnzvvo <= 2#0.0_1_1_1_1# us;
  
  -- Multi-driven assignments
  nttsw <= nttsw;
  wdppf <= nttsw;
  wdppf <= (others => 'W');
end ijxm;



-- Seed after: 10723973915736510877,12143220691580258643
