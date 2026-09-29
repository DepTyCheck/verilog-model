-- Seed: 10468222913548950599,10940991575366938685

library ieee;
use ieee.std_logic_1164.all;

entity khcaknx is
  port (hcx : in bit; uiyjqj : inout std_logic_vector(2 downto 2); oai : inout real; g : inout integer);
end khcaknx;

architecture ii of khcaknx is
  
begin
  -- Single-driven assignments
  g <= 2#0_0#;
  
  -- Multi-driven assignments
  uiyjqj <= (others => 'X');
end ii;

library ieee;
use ieee.std_logic_1164.all;

entity sficqns is
  port (gadwzjvqv : buffer time; tzht : buffer time_vector(0 downto 1); jkdrjpd : inout string(1 to 2); g : inout std_logic_vector(3 downto 0));
end sficqns;

library ieee;
use ieee.std_logic_1164.all;

architecture xp of sficqns is
  signal nzitlnqpoh : integer;
  signal lcnmgoklyk : real;
  signal pghpknastf : std_logic_vector(2 downto 2);
  signal aqigbem : bit;
begin
  zkfxrq : entity work.khcaknx
    port map (hcx => aqigbem, uiyjqj => pghpknastf, oai => lcnmgoklyk, g => nzitlnqpoh);
  
  -- Single-driven assignments
  jkdrjpd <= jkdrjpd;
end xp;

entity wjcckwg is
  port (w : buffer integer_vector(1 to 1); aqlbshpq : buffer time);
end wjcckwg;

library ieee;
use ieee.std_logic_1164.all;

architecture gjuvniwqt of wjcckwg is
  signal dd : std_logic_vector(3 downto 0);
  signal rk : string(1 to 2);
  signal ppmqau : time_vector(0 downto 1);
  signal e : integer;
  signal syfm : real;
  signal ekg : std_logic_vector(2 downto 2);
  signal n : bit;
  signal tuegslvevh : integer;
  signal siwbjhoe : real;
  signal pyu : std_logic_vector(2 downto 2);
  signal ggzjh : bit;
  signal t : std_logic_vector(3 downto 0);
  signal mm : string(1 to 2);
  signal sliqsfic : time_vector(0 downto 1);
  signal ew : time;
begin
  gpppt : entity work.sficqns
    port map (gadwzjvqv => ew, tzht => sliqsfic, jkdrjpd => mm, g => t);
  huodzxr : entity work.khcaknx
    port map (hcx => ggzjh, uiyjqj => pyu, oai => siwbjhoe, g => tuegslvevh);
  yrvtnwu : entity work.khcaknx
    port map (hcx => n, uiyjqj => ekg, oai => syfm, g => e);
  tmocdyuqq : entity work.sficqns
    port map (gadwzjvqv => aqlbshpq, tzht => ppmqau, jkdrjpd => rk, g => dd);
  
  -- Multi-driven assignments
  t <= ('U', '-', 'L', '1');
  dd <= "1U-0";
  dd <= t;
  t <= t;
end gjuvniwqt;



-- Seed after: 3828647315023463432,10940991575366938685
