-- Seed: 3251618794290334802,16715549879197889543



entity wwywl is
  port (fpl : out time_vector(4 downto 2));
end wwywl;



architecture re of wwywl is
  
begin
  
end re;

library ieee;
use ieee.std_logic_1164.all;

entity dopio is
  port (q : buffer std_logic_vector(1 downto 0));
end dopio;



architecture nxek of dopio is
  signal lcnboun : time_vector(4 downto 2);
begin
  xrc : entity work.wwywl
    port map (fpl => lcnboun);
end nxek;

library ieee;
use ieee.std_logic_1164.all;

entity sd is
  port (eusmpdmm : linkage severity_level; lrpk : inout std_logic_vector(3 to 2); gvz : in integer; r : inout std_logic);
end sd;



architecture oauewx of sd is
  signal wjke : time_vector(4 downto 2);
  signal gjwp : time_vector(4 downto 2);
begin
  phd : entity work.wwywl
    port map (fpl => gjwp);
  jgrora : entity work.wwywl
    port map (fpl => wjke);
end oauewx;

library ieee;
use ieee.std_logic_1164.all;

entity olqp is
  port (utzbr : inout real; ctgvvcpz : out time; xnhcz : in bit; rumezzfb : out std_logic_vector(4 downto 3));
end olqp;



architecture g of olqp is
  signal abxqbcpepi : time_vector(4 downto 2);
  signal bavxq : time_vector(4 downto 2);
  signal djqwveeiyu : time_vector(4 downto 2);
  signal swmmzkyjk : time_vector(4 downto 2);
begin
  gyupweq : entity work.wwywl
    port map (fpl => swmmzkyjk);
  agfovff : entity work.wwywl
    port map (fpl => djqwveeiyu);
  qs : entity work.wwywl
    port map (fpl => bavxq);
  oz : entity work.wwywl
    port map (fpl => abxqbcpepi);
end g;



-- Seed after: 2854131357877450472,16715549879197889543
