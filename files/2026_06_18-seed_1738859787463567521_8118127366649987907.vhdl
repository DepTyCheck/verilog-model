-- Seed: 1738859787463567521,8118127366649987907

library ieee;
use ieee.std_logic_1164.all;

entity glffftuect is
  port (axjyzq : linkage std_logic; fg : out std_logic);
end glffftuect;

architecture pfmqbjpvd of glffftuect is
  
begin
  -- Multi-driven assignments
  fg <= 'H';
  fg <= '1';
  fg <= 'L';
  fg <= 'L';
end pfmqbjpvd;

library ieee;
use ieee.std_logic_1164.all;

entity lrdjncz is
  port (kqowiw : buffer std_logic; af : inout std_logic_vector(0 downto 2));
end lrdjncz;

library ieee;
use ieee.std_logic_1164.all;

architecture uh of lrdjncz is
  signal ibvbllei : std_logic;
begin
  jx : entity work.glffftuect
    port map (axjyzq => kqowiw, fg => kqowiw);
  kfya : entity work.glffftuect
    port map (axjyzq => kqowiw, fg => ibvbllei);
  
  -- Multi-driven assignments
  af <= (others => '0');
end uh;

library ieee;
use ieee.std_logic_1164.all;

entity kbsgyr is
  port (hgfyl : inout real; kjgcje : inout real_vector(1 to 1); gvxzqnautz : linkage std_logic; zfqzgui : buffer time);
end kbsgyr;

library ieee;
use ieee.std_logic_1164.all;

architecture ygiuwzy of kbsgyr is
  signal srbfvjh : std_logic_vector(0 downto 2);
  signal tltxth : std_logic;
begin
  hvxdqkb : entity work.lrdjncz
    port map (kqowiw => tltxth, af => srbfvjh);
  
  -- Single-driven assignments
  zfqzgui <= 4 min;
  
  -- Multi-driven assignments
  srbfvjh <= (others => '0');
  tltxth <= 'X';
end ygiuwzy;

entity dkzcew is
  port (vuzuntnrc : buffer character; jkfr : linkage severity_level);
end dkzcew;

library ieee;
use ieee.std_logic_1164.all;

architecture ka of dkzcew is
  signal kegfqpha : std_logic_vector(0 downto 2);
  signal y : std_logic;
begin
  hkup : entity work.glffftuect
    port map (axjyzq => y, fg => y);
  rqvzo : entity work.lrdjncz
    port map (kqowiw => y, af => kegfqpha);
  bbaq : entity work.glffftuect
    port map (axjyzq => y, fg => y);
  ofwcxml : entity work.glffftuect
    port map (axjyzq => y, fg => y);
  
  -- Single-driven assignments
  vuzuntnrc <= 'v';
  
  -- Multi-driven assignments
  kegfqpha <= (others => '0');
  kegfqpha <= (others => '0');
  kegfqpha <= (others => '0');
  y <= 'W';
end ka;



-- Seed after: 13777186699919877871,8118127366649987907
