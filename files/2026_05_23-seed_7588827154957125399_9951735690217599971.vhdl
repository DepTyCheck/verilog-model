-- Seed: 7588827154957125399,9951735690217599971

library ieee;
use ieee.std_logic_1164.all;

entity ecmzuadeui is
  port (bjvkoidh : in std_logic; e : inout boolean_vector(0 downto 3); sfsxwau : linkage integer; ghmgff : buffer std_logic);
end ecmzuadeui;



architecture chdql of ecmzuadeui is
  
begin
  
end chdql;

library ieee;
use ieee.std_logic_1164.all;

entity ovwrp is
  port (rfnqy : buffer time; bwzkd : linkage std_logic_vector(0 downto 2));
end ovwrp;

library ieee;
use ieee.std_logic_1164.all;

architecture t of ovwrp is
  signal ogaxswc : boolean_vector(0 downto 3);
  signal oeflwpf : std_logic;
  signal l : boolean_vector(0 downto 3);
  signal rgvygekaae : std_logic;
  signal kkkzkhcaqz : boolean_vector(0 downto 3);
  signal qxmguhrl : std_logic;
  signal ct : std_logic;
  signal arzh : integer;
  signal wlohmtwf : boolean_vector(0 downto 3);
  signal ctthu : std_logic;
begin
  uzmz : entity work.ecmzuadeui
    port map (bjvkoidh => ctthu, e => wlohmtwf, sfsxwau => arzh, ghmgff => ct);
  lycotqkiz : entity work.ecmzuadeui
    port map (bjvkoidh => qxmguhrl, e => kkkzkhcaqz, sfsxwau => arzh, ghmgff => rgvygekaae);
  foqajmc : entity work.ecmzuadeui
    port map (bjvkoidh => qxmguhrl, e => l, sfsxwau => arzh, ghmgff => oeflwpf);
  cmencivsjz : entity work.ecmzuadeui
    port map (bjvkoidh => ctthu, e => ogaxswc, sfsxwau => arzh, ghmgff => rgvygekaae);
end t;



-- Seed after: 12936340410873505371,9951735690217599971
