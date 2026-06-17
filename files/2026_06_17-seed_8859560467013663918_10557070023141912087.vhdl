-- Seed: 8859560467013663918,10557070023141912087

entity hxvp is
  port (etdr : inout real_vector(0 downto 4); sklk : in bit; wdnexlukhg : in integer_vector(4 to 0));
end hxvp;

architecture ecyuc of hxvp is
  
begin
  -- Single-driven assignments
  etdr <= (others => 0.0);
end ecyuc;

library ieee;
use ieee.std_logic_1164.all;

entity ondbp is
  port (r : buffer std_logic; legskpqir : linkage std_logic_vector(2 to 0); ehvveifno : buffer std_logic_vector(0 downto 4));
end ondbp;

architecture iqgxj of ondbp is
  signal rdgmhi : integer_vector(4 to 0);
  signal b : bit;
  signal jplvadc : real_vector(0 downto 4);
  signal jc : integer_vector(4 to 0);
  signal slwyky : bit;
  signal rlcvfytyyu : real_vector(0 downto 4);
  signal iyazmsb : integer_vector(4 to 0);
  signal mbssprm : bit;
  signal wrwpulo : real_vector(0 downto 4);
begin
  bgnt : entity work.hxvp
    port map (etdr => wrwpulo, sklk => mbssprm, wdnexlukhg => iyazmsb);
  kbwez : entity work.hxvp
    port map (etdr => rlcvfytyyu, sklk => slwyky, wdnexlukhg => jc);
  ydkdvu : entity work.hxvp
    port map (etdr => jplvadc, sklk => b, wdnexlukhg => rdgmhi);
  
  -- Multi-driven assignments
  r <= 'H';
  ehvveifno <= (others => '0');
end iqgxj;

entity a is
  port (fiwtwnr : buffer severity_level);
end a;

architecture mejm of a is
  signal tvigyo : integer_vector(4 to 0);
  signal k : real_vector(0 downto 4);
  signal sxutqx : bit;
  signal as : real_vector(0 downto 4);
  signal xhbif : integer_vector(4 to 0);
  signal gh : bit;
  signal iy : real_vector(0 downto 4);
begin
  zxo : entity work.hxvp
    port map (etdr => iy, sklk => gh, wdnexlukhg => xhbif);
  myhqoht : entity work.hxvp
    port map (etdr => as, sklk => sxutqx, wdnexlukhg => xhbif);
  ifvenjzzo : entity work.hxvp
    port map (etdr => k, sklk => sxutqx, wdnexlukhg => tvigyo);
end mejm;

library ieee;
use ieee.std_logic_1164.all;

entity hy is
  port (wnjm : inout real_vector(3 downto 3); lrc : in std_logic; piraq : inout time; iufyfbgm : inout time_vector(4 downto 1));
end hy;

library ieee;
use ieee.std_logic_1164.all;

architecture xi of hy is
  signal fdzy : integer_vector(4 to 0);
  signal gojwno : bit;
  signal ofljahkzw : real_vector(0 downto 4);
  signal vmzi : std_logic_vector(0 downto 4);
  signal qizeqsotow : std_logic;
begin
  dwvuzjcmij : entity work.ondbp
    port map (r => qizeqsotow, legskpqir => vmzi, ehvveifno => vmzi);
  xwxhebv : entity work.hxvp
    port map (etdr => ofljahkzw, sklk => gojwno, wdnexlukhg => fdzy);
  
  -- Single-driven assignments
  piraq <= 2133.20204 ns;
end xi;



-- Seed after: 7413714612503322587,10557070023141912087
