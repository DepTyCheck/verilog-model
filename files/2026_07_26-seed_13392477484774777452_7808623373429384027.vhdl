-- Seed: 13392477484774777452,7808623373429384027

library ieee;
use ieee.std_logic_1164.all;

entity sa is
  port (indcs : out std_logic_vector(3 to 0); sqmjrnl : linkage std_logic_vector(3 to 4); wuhohwmh : inout bit_vector(3 downto 0));
end sa;

architecture yfobkcnxgf of sa is
  
begin
  -- Single-driven assignments
  wuhohwmh <= ('1', '0', '0', '0');
  
  -- Multi-driven assignments
  indcs <= indcs;
  indcs <= indcs;
  indcs <= indcs;
  indcs <= "";
end yfobkcnxgf;

library ieee;
use ieee.std_logic_1164.all;

entity zjqjbodoj is
  port (pi : inout std_logic);
end zjqjbodoj;

library ieee;
use ieee.std_logic_1164.all;

architecture xy of zjqjbodoj is
  signal xpqjc : bit_vector(3 downto 0);
  signal gtbahrxgf : std_logic_vector(3 to 0);
  signal rbm : bit_vector(3 downto 0);
  signal ybnwoq : std_logic_vector(3 to 0);
  signal a : bit_vector(3 downto 0);
  signal dbfxjq : std_logic_vector(3 to 4);
  signal ncnqxfwx : bit_vector(3 downto 0);
  signal bxm : std_logic_vector(3 to 4);
  signal ufkc : std_logic_vector(3 to 0);
begin
  zmvrq : entity work.sa
    port map (indcs => ufkc, sqmjrnl => bxm, wuhohwmh => ncnqxfwx);
  ijznai : entity work.sa
    port map (indcs => ufkc, sqmjrnl => dbfxjq, wuhohwmh => a);
  x : entity work.sa
    port map (indcs => ybnwoq, sqmjrnl => dbfxjq, wuhohwmh => rbm);
  y : entity work.sa
    port map (indcs => gtbahrxgf, sqmjrnl => bxm, wuhohwmh => xpqjc);
  
  -- Multi-driven assignments
  ybnwoq <= ybnwoq;
end xy;

library ieee;
use ieee.std_logic_1164.all;

entity ofu is
  port (muwurt : linkage bit; icm : linkage std_logic_vector(1 to 1); xxhtbpxh : out time);
end ofu;

library ieee;
use ieee.std_logic_1164.all;

architecture jdhjr of ofu is
  signal byurdz : bit_vector(3 downto 0);
  signal auigj : std_logic_vector(3 to 4);
  signal wl : std_logic_vector(3 to 0);
  signal tnk : bit_vector(3 downto 0);
  signal zzmshpdst : std_logic_vector(3 to 4);
  signal jebvec : std_logic_vector(3 to 0);
begin
  kincqu : entity work.sa
    port map (indcs => jebvec, sqmjrnl => zzmshpdst, wuhohwmh => tnk);
  gkfyxw : entity work.sa
    port map (indcs => wl, sqmjrnl => auigj, wuhohwmh => byurdz);
  
  -- Single-driven assignments
  xxhtbpxh <= xxhtbpxh;
  
  -- Multi-driven assignments
  zzmshpdst <= ('L', 'Z');
  jebvec <= jebvec;
  jebvec <= (others => '0');
  wl <= "";
end jdhjr;



-- Seed after: 15664138743076382157,7808623373429384027
