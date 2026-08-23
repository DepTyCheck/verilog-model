-- Seed: 11999520949227688763,4245627776430562977

library ieee;
use ieee.std_logic_1164.all;

entity fmdgk is
  port (uizy : linkage time; hgefabun : in std_logic; lkyidjei : in std_logic_vector(0 to 4));
end fmdgk;

architecture ajgyo of fmdgk is
  
begin
  
end ajgyo;

library ieee;
use ieee.std_logic_1164.all;

entity soldilz is
  port (ejz : buffer std_logic_vector(1 to 4); igzm : inout real);
end soldilz;

library ieee;
use ieee.std_logic_1164.all;

architecture fxceldm of soldilz is
  signal cv : std_logic;
  signal yyhmnntwy : time;
  signal bplbkk : std_logic;
  signal aumrc : time;
  signal ddored : std_logic_vector(0 to 4);
  signal mhzpuuy : std_logic;
  signal nelowtnxvs : time;
begin
  la : entity work.fmdgk
    port map (uizy => nelowtnxvs, hgefabun => mhzpuuy, lkyidjei => ddored);
  rmxlw : entity work.fmdgk
    port map (uizy => aumrc, hgefabun => bplbkk, lkyidjei => ddored);
  vnluqmv : entity work.fmdgk
    port map (uizy => yyhmnntwy, hgefabun => cv, lkyidjei => ddored);
  
  -- Multi-driven assignments
  ejz <= "1X-Z";
end fxceldm;

library ieee;
use ieee.std_logic_1164.all;

entity urlkx is
  port (m : buffer std_logic; jrrc : out time; shbxefwhyn : buffer std_logic; gjqnwso : inout std_logic_vector(4 downto 0));
end urlkx;

library ieee;
use ieee.std_logic_1164.all;

architecture auqysg of urlkx is
  signal v : std_logic_vector(0 to 4);
  signal kdc : std_logic;
  signal by : time;
begin
  cogsxxp : entity work.fmdgk
    port map (uizy => by, hgefabun => kdc, lkyidjei => gjqnwso);
  bczk : entity work.fmdgk
    port map (uizy => jrrc, hgefabun => shbxefwhyn, lkyidjei => v);
  
  -- Multi-driven assignments
  kdc <= 'X';
  gjqnwso <= gjqnwso;
  gjqnwso <= v;
end auqysg;

library ieee;
use ieee.std_logic_1164.all;

entity sugdkvzfmp is
  port (qlei : buffer std_logic; i : in integer; ajdeiqqngj : inout real; jcgeixnrn : linkage std_logic);
end sugdkvzfmp;

library ieee;
use ieee.std_logic_1164.all;

architecture ibegulobx of sugdkvzfmp is
  signal fyukhleaq : time;
  signal h : std_logic_vector(0 to 4);
  signal bb : time;
  signal wbdpnwco : real;
  signal gyqeuugqj : std_logic_vector(1 to 4);
begin
  p : entity work.soldilz
    port map (ejz => gyqeuugqj, igzm => wbdpnwco);
  tz : entity work.fmdgk
    port map (uizy => bb, hgefabun => qlei, lkyidjei => h);
  nhmxxain : entity work.fmdgk
    port map (uizy => fyukhleaq, hgefabun => qlei, lkyidjei => h);
  
  -- Single-driven assignments
  ajdeiqqngj <= ajdeiqqngj;
  
  -- Multi-driven assignments
  qlei <= qlei;
  h <= ('X', 'W', 'L', '-', 'U');
end ibegulobx;



-- Seed after: 10291730728239935103,4245627776430562977
