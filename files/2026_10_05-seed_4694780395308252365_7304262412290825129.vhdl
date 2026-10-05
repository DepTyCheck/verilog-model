-- Seed: 4694780395308252365,7304262412290825129

library ieee;
use ieee.std_logic_1164.all;

entity hhxngxw is
  port (ta : inout std_logic_vector(2 to 3); rv : out std_logic; c : inout time_vector(1 downto 4));
end hhxngxw;

architecture ywwgdj of hhxngxw is
  
begin
  -- Single-driven assignments
  c <= (others => 0 ns);
  
  -- Multi-driven assignments
  rv <= 'X';
end ywwgdj;

library ieee;
use ieee.std_logic_1164.all;

entity xquaxloh is
  port (rtbqcohafx : inout std_logic; mb : buffer std_logic; kfhkxxuapx : inout severity_level);
end xquaxloh;

library ieee;
use ieee.std_logic_1164.all;

architecture bsualsd of xquaxloh is
  signal tqe : time_vector(1 downto 4);
  signal moncpw : std_logic;
  signal pjiz : time_vector(1 downto 4);
  signal kniwhlzqix : std_logic;
  signal psowuzmt : std_logic_vector(2 to 3);
  signal mqpenna : time_vector(1 downto 4);
  signal zo : std_logic;
  signal ev : std_logic_vector(2 to 3);
  signal iiefyh : time_vector(1 downto 4);
  signal dtqroeff : std_logic_vector(2 to 3);
begin
  te : entity work.hhxngxw
    port map (ta => dtqroeff, rv => mb, c => iiefyh);
  xb : entity work.hhxngxw
    port map (ta => ev, rv => zo, c => mqpenna);
  suiw : entity work.hhxngxw
    port map (ta => psowuzmt, rv => kniwhlzqix, c => pjiz);
  bk : entity work.hhxngxw
    port map (ta => dtqroeff, rv => moncpw, c => tqe);
  
  -- Multi-driven assignments
  zo <= 'W';
  mb <= 'L';
end bsualsd;

library ieee;
use ieee.std_logic_1164.all;

entity vkuuh is
  port (wz : buffer integer; hnuliq : in std_logic);
end vkuuh;

architecture igikzvdf of vkuuh is
  
begin
  -- Single-driven assignments
  wz <= wz;
end igikzvdf;



-- Seed after: 1073321711976838213,7304262412290825129
