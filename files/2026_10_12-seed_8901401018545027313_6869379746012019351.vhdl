-- Seed: 8901401018545027313,6869379746012019351

library ieee;
use ieee.std_logic_1164.all;

entity hffeipo is
  port (khlxaj : out time_vector(3 to 2); dfqkutcu : in bit; wh : linkage std_logic_vector(2 downto 3); xqo : linkage std_logic_vector(1 downto 3));
end hffeipo;

architecture eq of hffeipo is
  
begin
  -- Single-driven assignments
  khlxaj <= khlxaj;
end eq;

entity v is
  port (lmrhyt : in real; trfhtj : linkage real);
end v;

library ieee;
use ieee.std_logic_1164.all;

architecture qoqd of v is
  signal vbyhqsuxm : time_vector(3 to 2);
  signal rtat : std_logic_vector(2 downto 3);
  signal pdcfzxg : bit;
  signal prelgdicey : time_vector(3 to 2);
  signal mdqqbgib : std_logic_vector(1 downto 3);
  signal n : bit;
  signal khlliv : time_vector(3 to 2);
  signal sp : std_logic_vector(1 downto 3);
  signal pzmw : bit;
  signal osxv : time_vector(3 to 2);
begin
  pnehmv : entity work.hffeipo
    port map (khlxaj => osxv, dfqkutcu => pzmw, wh => sp, xqo => sp);
  px : entity work.hffeipo
    port map (khlxaj => khlliv, dfqkutcu => n, wh => sp, xqo => mdqqbgib);
  tyuodyns : entity work.hffeipo
    port map (khlxaj => prelgdicey, dfqkutcu => pdcfzxg, wh => rtat, xqo => sp);
  htwspaqdbh : entity work.hffeipo
    port map (khlxaj => vbyhqsuxm, dfqkutcu => n, wh => sp, xqo => sp);
  
  -- Single-driven assignments
  n <= '0';
  pdcfzxg <= n;
end qoqd;



-- Seed after: 3875299401967666684,6869379746012019351
