-- Seed: 14643317630393006339,7304262412290825129

library ieee;
use ieee.std_logic_1164.all;

entity jognzvrkz is
  port (zqy : buffer std_logic_vector(1 to 3); d : linkage real_vector(1 downto 3));
end jognzvrkz;

architecture vzcwlk of jognzvrkz is
  
begin
  
end vzcwlk;

library ieee;
use ieee.std_logic_1164.all;

entity ifmnnibyo is
  port (zipmgngkq : out std_logic; rpltdkka : in string(2 to 1); gpio : in std_logic);
end ifmnnibyo;

library ieee;
use ieee.std_logic_1164.all;

architecture isab of ifmnnibyo is
  signal luwzk : real_vector(1 downto 3);
  signal umul : std_logic_vector(1 to 3);
  signal uaoenhh : real_vector(1 downto 3);
  signal zsuxgwcfe : std_logic_vector(1 to 3);
begin
  ftocgitn : entity work.jognzvrkz
    port map (zqy => zsuxgwcfe, d => uaoenhh);
  kinfwdfgzp : entity work.jognzvrkz
    port map (zqy => umul, d => luwzk);
  
  -- Multi-driven assignments
  umul <= ('1', 'W', 'W');
  umul <= zsuxgwcfe;
  umul <= ('H', 'H', '1');
  zipmgngkq <= gpio;
end isab;



-- Seed after: 9581926941192526224,7304262412290825129
