-- Seed: 3878193859616222101,6299883410057943775

library ieee;
use ieee.std_logic_1164.all;

entity ibwkgoagq is
  port (wkrfwu : inout time; lecv : in bit_vector(0 to 3); wc : out std_logic_vector(3 to 3); ewyonmodf : inout time);
end ibwkgoagq;

architecture jf of ibwkgoagq is
  
begin
  -- Multi-driven assignments
  wc <= (others => 'L');
  wc <= (others => 'X');
  wc <= wc;
  wc <= wc;
end jf;

entity iifp is
  port (gw : out time; hxgd : buffer character; oa : inout time);
end iifp;

library ieee;
use ieee.std_logic_1164.all;

architecture h of iifp is
  signal xueo : time;
  signal vwe : time;
  signal jjr : time;
  signal xa : std_logic_vector(3 to 3);
  signal e : bit_vector(0 to 3);
  signal fpnwcyj : time;
begin
  idlagkxug : entity work.ibwkgoagq
    port map (wkrfwu => fpnwcyj, lecv => e, wc => xa, ewyonmodf => jjr);
  rytynjid : entity work.ibwkgoagq
    port map (wkrfwu => gw, lecv => e, wc => xa, ewyonmodf => oa);
  yjpp : entity work.ibwkgoagq
    port map (wkrfwu => vwe, lecv => e, wc => xa, ewyonmodf => xueo);
  
  -- Multi-driven assignments
  xa <= xa;
end h;

entity uu is
  port (udt : linkage integer_vector(1 to 4));
end uu;

library ieee;
use ieee.std_logic_1164.all;

architecture qkqbf of uu is
  signal nfu : time;
  signal dpkuzs : std_logic_vector(3 to 3);
  signal jorjwwa : bit_vector(0 to 3);
  signal gp : time;
  signal hdfjpvek : time;
  signal qlfzfo : std_logic_vector(3 to 3);
  signal px : bit_vector(0 to 3);
  signal zrmxoc : time;
begin
  w : entity work.ibwkgoagq
    port map (wkrfwu => zrmxoc, lecv => px, wc => qlfzfo, ewyonmodf => hdfjpvek);
  eq : entity work.ibwkgoagq
    port map (wkrfwu => gp, lecv => jorjwwa, wc => dpkuzs, ewyonmodf => nfu);
  
  -- Single-driven assignments
  jorjwwa <= ('0', '1', '1', '0');
end qkqbf;

library ieee;
use ieee.std_logic_1164.all;

entity col is
  port (kgfqrb : linkage integer; okddc : in integer; ecvtubms : out std_logic_vector(3 downto 3); jjkgedd : linkage std_logic);
end col;

architecture jwml of col is
  signal wwrwtpbox : integer_vector(1 to 4);
  signal oit : time;
  signal lzexcao : time;
  signal tlgdaeutw : time;
  signal ganwdklxp : time;
  signal yiqqa : time;
  signal wo : bit_vector(0 to 3);
  signal bjvcejypdd : time;
begin
  fafksb : entity work.ibwkgoagq
    port map (wkrfwu => bjvcejypdd, lecv => wo, wc => ecvtubms, ewyonmodf => yiqqa);
  udwie : entity work.ibwkgoagq
    port map (wkrfwu => ganwdklxp, lecv => wo, wc => ecvtubms, ewyonmodf => tlgdaeutw);
  v : entity work.ibwkgoagq
    port map (wkrfwu => lzexcao, lecv => wo, wc => ecvtubms, ewyonmodf => oit);
  tytezdp : entity work.uu
    port map (udt => wwrwtpbox);
  
  -- Single-driven assignments
  wo <= wo;
  
  -- Multi-driven assignments
  ecvtubms <= ecvtubms;
  ecvtubms <= "Z";
  ecvtubms <= ecvtubms;
  ecvtubms <= "U";
end jwml;



-- Seed after: 1842271602368535612,6299883410057943775
