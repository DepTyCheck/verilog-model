-- Seed: 17854226249377124596,1641934135882347475

library ieee;
use ieee.std_logic_1164.all;

entity hcgwnnwz is
  port (o : out time_vector(3 downto 2); pojv : out std_logic; cufutgknq : buffer std_logic; vhxrvnra : linkage std_logic_vector(2 downto 1));
end hcgwnnwz;



architecture ad of hcgwnnwz is
  
begin
  
end ad;

library ieee;
use ieee.std_logic_1164.all;

entity puugjqhw is
  port (zzbd : linkage std_logic; jn : in std_logic_vector(3 downto 4));
end puugjqhw;



architecture dxtcpafaj of puugjqhw is
  
begin
  
end dxtcpafaj;



entity btq is
  port (xsncv : linkage character; dtsv : in integer; vzjgxulkc : inout boolean_vector(3 to 0));
end btq;

library ieee;
use ieee.std_logic_1164.all;

architecture rbcbcfsvp of btq is
  signal nop : std_logic;
  signal aabujsnv : time_vector(3 downto 2);
  signal kh : std_logic_vector(2 downto 1);
  signal hrzowkvwg : std_logic;
  signal aey : time_vector(3 downto 2);
  signal zxgihwhrc : std_logic_vector(3 downto 4);
  signal lwjjtfth : std_logic;
begin
  vh : entity work.puugjqhw
    port map (zzbd => lwjjtfth, jn => zxgihwhrc);
  xcgpioxc : entity work.hcgwnnwz
    port map (o => aey, pojv => hrzowkvwg, cufutgknq => lwjjtfth, vhxrvnra => kh);
  hvdqhcm : entity work.hcgwnnwz
    port map (o => aabujsnv, pojv => lwjjtfth, cufutgknq => nop, vhxrvnra => kh);
  nyykcu : entity work.puugjqhw
    port map (zzbd => lwjjtfth, jn => zxgihwhrc);
end rbcbcfsvp;



entity mup is
  port (swsgau : linkage real; qmze : inout real; vda : in integer_vector(0 downto 3));
end mup;

library ieee;
use ieee.std_logic_1164.all;

architecture ianyx of mup is
  signal dbfzwm : std_logic_vector(2 downto 1);
  signal yajw : time_vector(3 downto 2);
  signal bgheyxzes : boolean_vector(3 to 0);
  signal oqybywa : integer;
  signal rahmdfwa : character;
  signal xbkvibsw : std_logic_vector(3 downto 4);
  signal ohlcxwhgkq : std_logic;
begin
  ixkmgqm : entity work.puugjqhw
    port map (zzbd => ohlcxwhgkq, jn => xbkvibsw);
  wx : entity work.btq
    port map (xsncv => rahmdfwa, dtsv => oqybywa, vzjgxulkc => bgheyxzes);
  drbawkyebp : entity work.puugjqhw
    port map (zzbd => ohlcxwhgkq, jn => xbkvibsw);
  sxajuixbln : entity work.hcgwnnwz
    port map (o => yajw, pojv => ohlcxwhgkq, cufutgknq => ohlcxwhgkq, vhxrvnra => dbfzwm);
end ianyx;



-- Seed after: 6479327251547150529,1641934135882347475
