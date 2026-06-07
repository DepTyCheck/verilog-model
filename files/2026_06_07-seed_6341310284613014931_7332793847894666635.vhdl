-- Seed: 6341310284613014931,7332793847894666635

library ieee;
use ieee.std_logic_1164.all;

entity wfxnn is
  port (lgjgsfat : linkage bit_vector(3 downto 4); qqt : linkage std_logic; wng : out std_logic_vector(1 to 4); ykaetbpx : in real);
end wfxnn;



architecture xkk of wfxnn is
  
begin
  
end xkk;

library ieee;
use ieee.std_logic_1164.all;

entity jfuq is
  port (fvtqxvs : out time; uxv : buffer std_logic_vector(4 to 0); smqbz : linkage real; fpphcvo : out time_vector(4 downto 3));
end jfuq;

library ieee;
use ieee.std_logic_1164.all;

architecture zrfy of jfuq is
  signal rjahq : std_logic_vector(1 to 4);
  signal c : std_logic;
  signal fgcdyfzy : real;
  signal hdeo : std_logic;
  signal ueqmcek : bit_vector(3 downto 4);
  signal lmmkyi : real;
  signal ujircs : std_logic_vector(1 to 4);
  signal tem : std_logic;
  signal riuhqhidd : bit_vector(3 downto 4);
begin
  oix : entity work.wfxnn
    port map (lgjgsfat => riuhqhidd, qqt => tem, wng => ujircs, ykaetbpx => lmmkyi);
  vwmovecuzw : entity work.wfxnn
    port map (lgjgsfat => ueqmcek, qqt => hdeo, wng => ujircs, ykaetbpx => fgcdyfzy);
  qrtqbd : entity work.wfxnn
    port map (lgjgsfat => riuhqhidd, qqt => c, wng => rjahq, ykaetbpx => lmmkyi);
end zrfy;

library ieee;
use ieee.std_logic_1164.all;

entity teceity is
  port (hj : inout std_logic; ykp : linkage integer; goc : linkage real);
end teceity;

library ieee;
use ieee.std_logic_1164.all;

architecture djmydzgis of teceity is
  signal ulpntw : time_vector(4 downto 3);
  signal rnxoyiml : std_logic_vector(4 to 0);
  signal mkgkics : time;
  signal jkyoflib : real;
  signal l : std_logic_vector(1 to 4);
  signal tce : std_logic;
  signal a : bit_vector(3 downto 4);
  signal yurnddtq : real;
  signal dgrugk : std_logic_vector(1 to 4);
  signal ylzqqctwd : std_logic;
  signal d : bit_vector(3 downto 4);
begin
  zn : entity work.wfxnn
    port map (lgjgsfat => d, qqt => ylzqqctwd, wng => dgrugk, ykaetbpx => yurnddtq);
  dqym : entity work.wfxnn
    port map (lgjgsfat => a, qqt => tce, wng => l, ykaetbpx => jkyoflib);
  xcllljyl : entity work.jfuq
    port map (fvtqxvs => mkgkics, uxv => rnxoyiml, smqbz => goc, fpphcvo => ulpntw);
end djmydzgis;



-- Seed after: 13772100537846232287,7332793847894666635
