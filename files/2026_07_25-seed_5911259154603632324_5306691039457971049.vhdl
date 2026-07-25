-- Seed: 5911259154603632324,5306691039457971049

entity mnggsyl is
  port (q : inout time; v : in integer; vzsshq : buffer time_vector(0 downto 1));
end mnggsyl;

architecture o of mnggsyl is
  
begin
  -- Single-driven assignments
  vzsshq <= vzsshq;
  q <= 2_4_2.2 ms;
end o;

library ieee;
use ieee.std_logic_1164.all;

entity ql is
  port (iyfcx : linkage time; t : buffer std_logic; mqlvtj : in time);
end ql;

architecture zzhu of ql is
  signal djmrrqhils : time_vector(0 downto 1);
  signal hrzl : integer;
  signal btrycmfvto : time;
begin
  s : entity work.mnggsyl
    port map (q => btrycmfvto, v => hrzl, vzsshq => djmrrqhils);
  
  -- Multi-driven assignments
  t <= '-';
  t <= 'H';
end zzhu;

library ieee;
use ieee.std_logic_1164.all;

entity hbibrgnls is
  port (jghh : out character; wb : linkage integer; yt : buffer real; lklalpox : buffer std_logic);
end hbibrgnls;

library ieee;
use ieee.std_logic_1164.all;

architecture nrkdqgi of hbibrgnls is
  signal dkdjfzryf : time_vector(0 downto 1);
  signal y : integer;
  signal bml : time;
  signal rgio : std_logic;
  signal fyxvl : time;
begin
  bktfsipl : entity work.ql
    port map (iyfcx => fyxvl, t => rgio, mqlvtj => bml);
  fiypip : entity work.mnggsyl
    port map (q => bml, v => y, vzsshq => dkdjfzryf);
  
  -- Single-driven assignments
  y <= 1_1_0_1;
  yt <= yt;
  jghh <= 'e';
end nrkdqgi;

library ieee;
use ieee.std_logic_1164.all;

entity rqpdnftwhu is
  port (fs : in std_logic; ksnram : buffer std_logic_vector(2 downto 2));
end rqpdnftwhu;

library ieee;
use ieee.std_logic_1164.all;

architecture xynltvfc of rqpdnftwhu is
  signal ezwwmjohh : time_vector(0 downto 1);
  signal ychflmnj : integer;
  signal xxhhaqx : time;
  signal yeqcsw : time_vector(0 downto 1);
  signal eaxackf : time_vector(0 downto 1);
  signal xytskx : integer;
  signal rsvgdxod : time;
  signal hntrhilpo : time;
  signal pjzkuw : std_logic;
  signal mqv : time;
begin
  kqbgdsmx : entity work.ql
    port map (iyfcx => mqv, t => pjzkuw, mqlvtj => hntrhilpo);
  jbcbvppout : entity work.mnggsyl
    port map (q => rsvgdxod, v => xytskx, vzsshq => eaxackf);
  eg : entity work.mnggsyl
    port map (q => hntrhilpo, v => xytskx, vzsshq => yeqcsw);
  af : entity work.mnggsyl
    port map (q => xxhhaqx, v => ychflmnj, vzsshq => ezwwmjohh);
  
  -- Single-driven assignments
  ychflmnj <= xytskx;
  xytskx <= 3_0_1_3;
  
  -- Multi-driven assignments
  ksnram <= ksnram;
  pjzkuw <= '1';
end xynltvfc;



-- Seed after: 17979365712944029743,5306691039457971049
