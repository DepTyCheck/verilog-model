-- Seed: 13930486653462743678,8891552411914730853

library ieee;
use ieee.std_logic_1164.all;

entity itca is
  port (ptqros : out time; gmffezo : buffer time; kpcnykb : buffer bit_vector(0 to 4); oknnilrsuq : out std_logic);
end itca;

architecture gjvzfualci of itca is
  
begin
  -- Multi-driven assignments
  oknnilrsuq <= 'L';
  oknnilrsuq <= oknnilrsuq;
end gjvzfualci;

library ieee;
use ieee.std_logic_1164.all;

entity ifd is
  port (gi : linkage time; qcaxok : in string(4 downto 1); fifh : linkage std_logic_vector(3 downto 1); pidcnki : buffer std_logic_vector(4 downto 4));
end ifd;

library ieee;
use ieee.std_logic_1164.all;

architecture xdftb of ifd is
  signal seloqnqumo : std_logic;
  signal xwxh : bit_vector(0 to 4);
  signal bjvejvlr : time;
  signal h : time;
begin
  avdkmk : entity work.itca
    port map (ptqros => h, gmffezo => bjvejvlr, kpcnykb => xwxh, oknnilrsuq => seloqnqumo);
  
  -- Multi-driven assignments
  pidcnki <= (others => '1');
end xdftb;

entity kvhjxckid is
  port (iixt : buffer boolean_vector(3 downto 4); qmnrqzksa : buffer boolean; fwp : buffer bit);
end kvhjxckid;

library ieee;
use ieee.std_logic_1164.all;

architecture mdljriq of kvhjxckid is
  signal zexigffju : std_logic_vector(4 downto 4);
  signal uzstqs : std_logic_vector(3 downto 1);
  signal fbj : string(4 downto 1);
  signal rkbxyvia : time;
  signal ywqqejggka : std_logic;
  signal zbackpce : bit_vector(0 to 4);
  signal qqn : time;
  signal w : time;
begin
  lf : entity work.itca
    port map (ptqros => w, gmffezo => qqn, kpcnykb => zbackpce, oknnilrsuq => ywqqejggka);
  isnrekudho : entity work.ifd
    port map (gi => rkbxyvia, qcaxok => fbj, fifh => uzstqs, pidcnki => zexigffju);
  
  -- Single-driven assignments
  fwp <= '0';
  qmnrqzksa <= TRUE;
  
  -- Multi-driven assignments
  zexigffju <= zexigffju;
  uzstqs <= uzstqs;
  ywqqejggka <= ywqqejggka;
  uzstqs <= uzstqs;
end mdljriq;

library ieee;
use ieee.std_logic_1164.all;

entity mmqunk is
  port (awwu : linkage std_logic);
end mmqunk;

library ieee;
use ieee.std_logic_1164.all;

architecture v of mmqunk is
  signal oao : std_logic;
  signal nsmbisrig : bit_vector(0 to 4);
  signal djhs : time;
  signal ipqjns : time;
  signal wrq : bit;
  signal vsdspk : boolean;
  signal pzmojjx : boolean_vector(3 downto 4);
begin
  buxxsyep : entity work.kvhjxckid
    port map (iixt => pzmojjx, qmnrqzksa => vsdspk, fwp => wrq);
  mmdxlm : entity work.itca
    port map (ptqros => ipqjns, gmffezo => djhs, kpcnykb => nsmbisrig, oknnilrsuq => oao);
  
  -- Multi-driven assignments
  oao <= 'W';
  oao <= '-';
  oao <= oao;
  oao <= oao;
end v;



-- Seed after: 1455425877184128609,8891552411914730853
