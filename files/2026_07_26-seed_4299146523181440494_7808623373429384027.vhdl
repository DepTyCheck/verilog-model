-- Seed: 4299146523181440494,7808623373429384027

library ieee;
use ieee.std_logic_1164.all;

entity kabr is
  port (hou : in std_logic_vector(3 downto 1));
end kabr;

architecture bp of kabr is
  
begin
  
end bp;

entity sdz is
  port (oiwlz : in real; zvwbpod : out real);
end sdz;

library ieee;
use ieee.std_logic_1164.all;

architecture njg of sdz is
  signal yepza : std_logic_vector(3 downto 1);
  signal zb : std_logic_vector(3 downto 1);
begin
  av : entity work.kabr
    port map (hou => zb);
  it : entity work.kabr
    port map (hou => yepza);
  
  -- Single-driven assignments
  zvwbpod <= 16#7.9_0_8_4#;
  
  -- Multi-driven assignments
  yepza <= zb;
  zb <= ('L', 'H', '1');
end njg;

entity ungtvrf is
  port (bwxn : in character; wpaoeisga : out real; xdwbaf : linkage time);
end ungtvrf;

library ieee;
use ieee.std_logic_1164.all;

architecture etjis of ungtvrf is
  signal wvlb : std_logic_vector(3 downto 1);
begin
  prxavah : entity work.sdz
    port map (oiwlz => wpaoeisga, zvwbpod => wpaoeisga);
  gsrkoaadff : entity work.kabr
    port map (hou => wvlb);
  
  -- Multi-driven assignments
  wvlb <= ('H', 'W', 'U');
  wvlb <= wvlb;
  wvlb <= "00-";
  wvlb <= wvlb;
end etjis;

library ieee;
use ieee.std_logic_1164.all;

entity vzym is
  port (t : linkage std_logic_vector(4 downto 4); pqfqo : linkage real);
end vzym;

library ieee;
use ieee.std_logic_1164.all;

architecture bgzbe of vzym is
  signal ayhxilcy : std_logic_vector(3 downto 1);
begin
  gzkcwqwt : entity work.kabr
    port map (hou => ayhxilcy);
  krzpeghe : entity work.kabr
    port map (hou => ayhxilcy);
  
  -- Multi-driven assignments
  ayhxilcy <= "UW0";
  ayhxilcy <= ayhxilcy;
end bgzbe;



-- Seed after: 5428113953901228014,7808623373429384027
