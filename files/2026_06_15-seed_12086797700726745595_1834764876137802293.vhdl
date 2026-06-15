-- Seed: 12086797700726745595,1834764876137802293

library ieee;
use ieee.std_logic_1164.all;

entity tjtmmzvttv is
  port (si : linkage std_logic_vector(4 to 2); bdqqly : out time; htblgs : in time);
end tjtmmzvttv;

architecture nmcblmpm of tjtmmzvttv is
  
begin
  -- Single-driven assignments
  bdqqly <= 4 sec;
end nmcblmpm;

entity ezmeqskfiv is
  port (zrup : buffer bit_vector(1 to 0); mimv : out time_vector(2 downto 4));
end ezmeqskfiv;

library ieee;
use ieee.std_logic_1164.all;

architecture akbt of ezmeqskfiv is
  signal s : time;
  signal dz : time;
  signal patabmwchb : std_logic_vector(4 to 2);
  signal w : time;
  signal lh : std_logic_vector(4 to 2);
begin
  sksryx : entity work.tjtmmzvttv
    port map (si => lh, bdqqly => w, htblgs => w);
  uod : entity work.tjtmmzvttv
    port map (si => patabmwchb, bdqqly => dz, htblgs => w);
  jg : entity work.tjtmmzvttv
    port map (si => patabmwchb, bdqqly => s, htblgs => w);
  
  -- Single-driven assignments
  mimv <= (others => 0 ns);
  
  -- Multi-driven assignments
  lh <= "";
end akbt;

library ieee;
use ieee.std_logic_1164.all;

entity dyf is
  port (lemvp : out real_vector(4 downto 4); uepyjfl : out std_logic; zjqecpis : inout time_vector(2 downto 4));
end dyf;

library ieee;
use ieee.std_logic_1164.all;

architecture w of dyf is
  signal naw : bit_vector(1 to 0);
  signal foawa : time;
  signal pjntht : time;
  signal hwjtx : time;
  signal rle : std_logic_vector(4 to 2);
begin
  lqvgekhxg : entity work.tjtmmzvttv
    port map (si => rle, bdqqly => hwjtx, htblgs => pjntht);
  phwpn : entity work.tjtmmzvttv
    port map (si => rle, bdqqly => foawa, htblgs => foawa);
  s : entity work.ezmeqskfiv
    port map (zrup => naw, mimv => zjqecpis);
  oake : entity work.tjtmmzvttv
    port map (si => rle, bdqqly => pjntht, htblgs => hwjtx);
  
  -- Multi-driven assignments
  rle <= "";
end w;

entity diodwqno is
  port (ynlfcwgici : linkage integer; swylq : buffer bit);
end diodwqno;

architecture dkcyst of diodwqno is
  
begin
  -- Single-driven assignments
  swylq <= '1';
end dkcyst;



-- Seed after: 13537199189558054483,1834764876137802293
