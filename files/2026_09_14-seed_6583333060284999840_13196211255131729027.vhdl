-- Seed: 6583333060284999840,13196211255131729027

library ieee;
use ieee.std_logic_1164.all;

entity yw is
  port (xh : in real; z : inout time; vcjrqk : linkage real; erx : linkage std_logic_vector(4 to 4));
end yw;

architecture nwnxho of yw is
  
begin
  
end nwnxho;

library ieee;
use ieee.std_logic_1164.all;

entity hvi is
  port (rmdana : out time; wbvawtqga : in real; fjpn : linkage std_logic_vector(3 downto 4));
end hvi;

architecture isuxhnd of hvi is
  
begin
  -- Single-driven assignments
  rmdana <= 1 hr;
end isuxhnd;

library ieee;
use ieee.std_logic_1164.all;

entity tc is
  port (encyrb : inout integer; yi : linkage std_logic; vpejgq : inout real);
end tc;

library ieee;
use ieee.std_logic_1164.all;

architecture sxznv of tc is
  signal ujeq : std_logic_vector(4 to 4);
  signal jkunwbmjv : time;
  signal dhe : real;
  signal sfhava : time;
  signal hrd : std_logic_vector(4 to 4);
  signal xenvacwao : real;
  signal x : time;
  signal gekyxafqtp : std_logic_vector(3 downto 4);
  signal hjmz : real;
  signal px : time;
begin
  vvzbmhcyci : entity work.hvi
    port map (rmdana => px, wbvawtqga => hjmz, fjpn => gekyxafqtp);
  vjwbcyolb : entity work.yw
    port map (xh => vpejgq, z => x, vcjrqk => xenvacwao, erx => hrd);
  ran : entity work.yw
    port map (xh => vpejgq, z => sfhava, vcjrqk => dhe, erx => hrd);
  xac : entity work.yw
    port map (xh => dhe, z => jkunwbmjv, vcjrqk => hjmz, erx => ujeq);
  
  -- Single-driven assignments
  vpejgq <= 42.3_2_0;
  
  -- Multi-driven assignments
  hrd <= hrd;
  hrd <= hrd;
end sxznv;



-- Seed after: 14659154718061508788,13196211255131729027
