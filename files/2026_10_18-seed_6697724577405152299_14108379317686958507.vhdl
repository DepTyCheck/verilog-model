-- Seed: 6697724577405152299,14108379317686958507

library ieee;
use ieee.std_logic_1164.all;

entity cmxslsrw is
  port (ch : inout character; yxnjxh : inout std_logic; lrthkv : in integer_vector(1 downto 2));
end cmxslsrw;

architecture xssbkr of cmxslsrw is
  
begin
  
end xssbkr;

entity bejrmqbb is
  port (psp : inout time; uzbelhhwwd : in time);
end bejrmqbb;

architecture c of bejrmqbb is
  
begin
  
end c;

entity e is
  port (zvb : buffer integer_vector(4 to 2); fgpuo : out time_vector(2 to 4));
end e;

library ieee;
use ieee.std_logic_1164.all;

architecture ebiuuxq of e is
  signal cbmsh : integer_vector(1 downto 2);
  signal tbj : character;
  signal orozkk : integer_vector(1 downto 2);
  signal kjzn : std_logic;
  signal imgw : character;
  signal oinnw : std_logic;
  signal s : character;
begin
  muqav : entity work.cmxslsrw
    port map (ch => s, yxnjxh => oinnw, lrthkv => zvb);
  qsyydl : entity work.cmxslsrw
    port map (ch => imgw, yxnjxh => kjzn, lrthkv => orozkk);
  zicnbidku : entity work.cmxslsrw
    port map (ch => tbj, yxnjxh => oinnw, lrthkv => cbmsh);
  
  -- Single-driven assignments
  cbmsh <= zvb;
  
  -- Multi-driven assignments
  kjzn <= oinnw;
  oinnw <= 'W';
end ebiuuxq;



-- Seed after: 15563064395779613264,14108379317686958507
