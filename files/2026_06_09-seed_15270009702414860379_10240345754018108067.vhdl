-- Seed: 15270009702414860379,10240345754018108067

library ieee;
use ieee.std_logic_1164.all;

entity migpqtcfav is
  port (t : buffer real; ounfprxcn : inout std_logic_vector(2 downto 0));
end migpqtcfav;



architecture fuvzfbvvn of migpqtcfav is
  
begin
  
end fuvzfbvvn;

library ieee;
use ieee.std_logic_1164.all;

entity b is
  port (xkxq : buffer real; egebkcnzbq : inout std_logic);
end b;

library ieee;
use ieee.std_logic_1164.all;

architecture lgnvfm of b is
  signal ws : std_logic_vector(2 downto 0);
  signal rfa : real;
begin
  nwisbaxtec : entity work.migpqtcfav
    port map (t => rfa, ounfprxcn => ws);
  avl : entity work.migpqtcfav
    port map (t => xkxq, ounfprxcn => ws);
end lgnvfm;

library ieee;
use ieee.std_logic_1164.all;

entity hxvvxeilkt is
  port (onxwnr : inout std_logic);
end hxvvxeilkt;

library ieee;
use ieee.std_logic_1164.all;

architecture uqy of hxvvxeilkt is
  signal oerg : real;
  signal tw : std_logic_vector(2 downto 0);
  signal ovp : real;
begin
  rx : entity work.migpqtcfav
    port map (t => ovp, ounfprxcn => tw);
  vxmzxopp : entity work.b
    port map (xkxq => oerg, egebkcnzbq => onxwnr);
end uqy;

library ieee;
use ieee.std_logic_1164.all;

entity cmjs is
  port (aohhnii : buffer real; dpbomklewe : inout std_logic_vector(2 downto 3); bocxgcjy : in std_logic_vector(3 to 3));
end cmjs;

library ieee;
use ieee.std_logic_1164.all;

architecture utskbeh of cmjs is
  signal w : std_logic;
  signal yuqx : std_logic;
begin
  unyvxsxen : entity work.hxvvxeilkt
    port map (onxwnr => yuqx);
  clsavwb : entity work.b
    port map (xkxq => aohhnii, egebkcnzbq => w);
end utskbeh;



-- Seed after: 11004998244176672226,10240345754018108067
