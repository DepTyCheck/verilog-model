-- Seed: 13049217192003257192,9900838946149614537

library ieee;
use ieee.std_logic_1164.all;

entity nuevc is
  port (vrjett : in std_logic_vector(3 to 1));
end nuevc;

architecture ps of nuevc is
  
begin
  
end ps;

entity vmlaytx is
  port (hi : buffer integer);
end vmlaytx;

library ieee;
use ieee.std_logic_1164.all;

architecture knxw of vmlaytx is
  signal jnt : std_logic_vector(3 to 1);
  signal wehyegm : std_logic_vector(3 to 1);
begin
  evoxqcm : entity work.nuevc
    port map (vrjett => wehyegm);
  xmnqlx : entity work.nuevc
    port map (vrjett => wehyegm);
  sqbpesf : entity work.nuevc
    port map (vrjett => jnt);
  pixprky : entity work.nuevc
    port map (vrjett => wehyegm);
  
  -- Multi-driven assignments
  wehyegm <= (others => '0');
  wehyegm <= wehyegm;
end knxw;

library ieee;
use ieee.std_logic_1164.all;

entity lmtul is
  port (ajsb : inout time; olbxkmcqp : inout time; xppdhe : buffer std_logic_vector(3 downto 3));
end lmtul;

library ieee;
use ieee.std_logic_1164.all;

architecture affxz of lmtul is
  signal uju : integer;
  signal amtmpvvrqb : integer;
  signal bbgwopx : std_logic_vector(3 to 1);
begin
  jjhgzdmi : entity work.nuevc
    port map (vrjett => bbgwopx);
  tuwzpunmnu : entity work.vmlaytx
    port map (hi => amtmpvvrqb);
  gkvqks : entity work.vmlaytx
    port map (hi => uju);
  
  -- Single-driven assignments
  olbxkmcqp <= olbxkmcqp;
  ajsb <= 40321 ps;
  
  -- Multi-driven assignments
  xppdhe <= xppdhe;
  xppdhe <= xppdhe;
  xppdhe <= xppdhe;
  bbgwopx <= (others => '0');
end affxz;

entity vvnkzlh is
  port (ainbgmpfpf : in time; yitr : linkage bit_vector(1 downto 4); mtr : in boolean);
end vvnkzlh;

library ieee;
use ieee.std_logic_1164.all;

architecture afp of vvnkzlh is
  signal hzhiqdb : std_logic_vector(3 downto 3);
  signal o : time;
  signal utug : time;
  signal grwljg : std_logic_vector(3 to 1);
begin
  ghe : entity work.nuevc
    port map (vrjett => grwljg);
  cmogz : entity work.nuevc
    port map (vrjett => grwljg);
  q : entity work.lmtul
    port map (ajsb => utug, olbxkmcqp => o, xppdhe => hzhiqdb);
  
  -- Multi-driven assignments
  hzhiqdb <= hzhiqdb;
  grwljg <= grwljg;
  grwljg <= grwljg;
end afp;



-- Seed after: 17352179824957314024,9900838946149614537
