-- Seed: 11195499173761530469,6140041381800297705

library ieee;
use ieee.std_logic_1164.all;

entity cae is
  port (atklpiqjw : out std_logic_vector(3 downto 4); pfdfmb : in time);
end cae;

architecture inuspkxth of cae is
  
begin
  -- Multi-driven assignments
  atklpiqjw <= "";
  atklpiqjw <= atklpiqjw;
  atklpiqjw <= "";
end inuspkxth;

entity irzxwacje is
  port (j : in integer; tmvynezz : out time; mpzlxmlm : in bit_vector(3 downto 3); utj : out integer);
end irzxwacje;

library ieee;
use ieee.std_logic_1164.all;

architecture rbouycxtg of irzxwacje is
  signal zxchraj : std_logic_vector(3 downto 4);
begin
  jy : entity work.cae
    port map (atklpiqjw => zxchraj, pfdfmb => tmvynezz);
  
  -- Single-driven assignments
  utj <= j;
  tmvynezz <= tmvynezz;
  
  -- Multi-driven assignments
  zxchraj <= zxchraj;
end rbouycxtg;

library ieee;
use ieee.std_logic_1164.all;

entity jzasunpeq is
  port (fnnyqpl : inout severity_level; evspuewjq : linkage std_logic_vector(3 to 3));
end jzasunpeq;

library ieee;
use ieee.std_logic_1164.all;

architecture mlkdxcyqt of jzasunpeq is
  signal y : integer;
  signal ixjrsxr : bit_vector(3 downto 3);
  signal ldwbwkn : integer;
  signal aqxyrzbyrj : time;
  signal hpoeyeltc : time;
  signal itll : std_logic_vector(3 downto 4);
begin
  aoglemaxro : entity work.cae
    port map (atklpiqjw => itll, pfdfmb => hpoeyeltc);
  fifayfw : entity work.cae
    port map (atklpiqjw => itll, pfdfmb => aqxyrzbyrj);
  qkokmlzx : entity work.irzxwacje
    port map (j => ldwbwkn, tmvynezz => hpoeyeltc, mpzlxmlm => ixjrsxr, utj => y);
  uectxpd : entity work.irzxwacje
    port map (j => ldwbwkn, tmvynezz => aqxyrzbyrj, mpzlxmlm => ixjrsxr, utj => ldwbwkn);
end mlkdxcyqt;

library ieee;
use ieee.std_logic_1164.all;

entity xtybxyfhez is
  port (tfop : in std_logic; wpczzxdvc : out real; bn : inout string(3 to 1); usxovw : in time_vector(0 to 3));
end xtybxyfhez;

library ieee;
use ieee.std_logic_1164.all;

architecture b of xtybxyfhez is
  signal kefmu : bit_vector(3 downto 3);
  signal mpex : time;
  signal tc : integer;
  signal glfe : time;
  signal zybvuakhoa : std_logic_vector(3 downto 4);
begin
  hfzhz : entity work.cae
    port map (atklpiqjw => zybvuakhoa, pfdfmb => glfe);
  slssi : entity work.irzxwacje
    port map (j => tc, tmvynezz => mpex, mpzlxmlm => kefmu, utj => tc);
  
  -- Single-driven assignments
  bn <= "";
  kefmu <= (others => '0');
  glfe <= glfe;
  wpczzxdvc <= 02.2_4_4_4_0;
  
  -- Multi-driven assignments
  zybvuakhoa <= zybvuakhoa;
end b;



-- Seed after: 269567777139360302,6140041381800297705
