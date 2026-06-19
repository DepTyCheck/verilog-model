-- Seed: 13981988984722235362,3108530264173481209

library ieee;
use ieee.std_logic_1164.all;

entity pm is
  port (ix : linkage std_logic_vector(3 downto 2); makffhue : inout std_logic; q : linkage real; wxvwyjcjcm : out integer_vector(3 downto 1));
end pm;

architecture fjyotm of pm is
  
begin
  -- Multi-driven assignments
  makffhue <= 'X';
  makffhue <= 'U';
end fjyotm;

library ieee;
use ieee.std_logic_1164.all;

entity zyocxxaht is
  port (wgnpt : buffer time; dclbxlh : buffer std_logic_vector(3 to 4));
end zyocxxaht;

library ieee;
use ieee.std_logic_1164.all;

architecture lhftou of zyocxxaht is
  signal wjscbr : integer_vector(3 downto 1);
  signal cwjscqohqu : real;
  signal snikded : std_logic;
  signal iltxoyfp : integer_vector(3 downto 1);
  signal hm : real;
  signal hdrsib : integer_vector(3 downto 1);
  signal clgtflwyr : real;
  signal dqumrf : std_logic;
begin
  fjsa : entity work.pm
    port map (ix => dclbxlh, makffhue => dqumrf, q => clgtflwyr, wxvwyjcjcm => hdrsib);
  uaa : entity work.pm
    port map (ix => dclbxlh, makffhue => dqumrf, q => hm, wxvwyjcjcm => iltxoyfp);
  nj : entity work.pm
    port map (ix => dclbxlh, makffhue => snikded, q => cwjscqohqu, wxvwyjcjcm => wjscbr);
  
  -- Single-driven assignments
  wgnpt <= 331 ms;
end lhftou;



-- Seed after: 1572398059890330677,3108530264173481209
